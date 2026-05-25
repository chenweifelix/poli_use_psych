# Setting ----
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(scales)
library(ggrepel)

set.seed(123)

# Load graph -----
g_poli_uni_largest = read_graph(
  "5-network/data/df_psych_network_largest_comp_nodelist.graphml",
  format = "graphml"
)

df_node = read_csv("5-network/out/python_sbm.csv")

vec_nodes = V(g_poli_uni_largest) %>% names()

# Reorder df to match graph vertex order
df_node =
  df_node %>%
  slice(match(vec_nodes, node))

vec_nodes = V(g_poli_uni_largest) %>% names()
# Map inferred class
V(g_poli_uni_largest)$membership = as.character(df_node$community)


# Cluster sizes ----
temp = df_node %>% pull(community) %>% table() %>% sort()
temp_mem_num =
  tibble(
    mem = names(temp),
    num_poli = as.integer(temp)
  ) %>%
  arrange(desc(num_poli))

top_10_cluster = temp_mem_num %>% slice_head(n = 10) %>% pull(mem)

# Keep only top-10 clusters
vid_cluster = V(g_poli_uni_largest)[V(g_poli_uni_largest)$membership %in% top_10_cluster]
g_cluster = induced_subgraph(g_poli_uni_largest, vids = vid_cluster)

# Make sure membership is character
V(g_cluster)$membership = as.character(V(g_cluster)$membership)


# Visualize ------
# --------------------------------------------------
# 1. Build a cluster-level graph to position clusters
# --------------------------------------------------
membership_vec = V(g_cluster)$membership

edges_comm =
  igraph::as_data_frame(g_cluster, what = "edges") %>%
  mutate(
    from_comm = membership_vec[match(.data$from, V(g_cluster)$name)],
    to_comm   = membership_vec[match(.data$to,   V(g_cluster)$name)]
  ) %>%
  filter(from_comm != to_comm) %>%
  transmute(
    c1 = pmin(from_comm, to_comm),
    c2 = pmax(from_comm, to_comm),
    weight = ifelse(is.na(weight), 1, weight)
  ) %>%
  count(c1, c2, wt = weight, name = "weight")

comm_vertices =
  tibble(name = unique(V(g_cluster)$membership)) %>%
  left_join(
    temp_mem_num %>%
      filter(mem %in% top_10_cluster) %>%
      transmute(name = mem, size = num_poli),
    by = "name"
  )

g_comm = graph_from_data_frame(
  d = edges_comm %>% transmute(from = c1, to = c2, weight = weight),
  vertices = comm_vertices,
  directed = FALSE
)

# Layout for cluster centers
comm_layout = layout_with_fr(
  g_comm,
  weights = E(g_comm)$weight,
  niter = 5000
)

df_name = read_csv("5-network/out/cluster_names.csv")
df_name = df_name %>% mutate(community = as.character(community))
comm_centers =
  tibble(
    membership = V(g_comm)$name,
    size = V(g_comm)$size,
    cx = comm_layout[, 1],
    cy = comm_layout[, 2]
  ) %>%
  mutate(
    # spread clusters apart substantially
    cx = rescale(cx, to = c(-25, 25)),
    cy = rescale(cy, to = c(-18, 18))
  ) %>% 
  left_join(df_name, by = c("membership" = "community"))

  # --------------------------------------------------
# Spatial neighbors among cluster centers
# --------------------------------------------------

k_near = 3   # each cluster is contrasted against its 3 nearest clusters

dist_mat = as.matrix(dist(comm_centers[, c("cx", "cy")]))
diag(dist_mat) = Inf

df_close_edges =
  purrr::map_dfr(seq_len(nrow(comm_centers)), function(i) {
    j = order(dist_mat[i, ])[1:k_near]
    tibble(
      from = comm_centers$membership[i],
      to   = comm_centers$membership[j],
      d    = dist_mat[i, j]
    )
  }) %>%
  mutate(
    a = pmin(from, to),
    b = pmax(from, to)
  ) %>%
  group_by(a, b) %>%
  summarize(
    d = min(d),
    .groups = "drop"
  ) %>%
  rename(from = a, to = b)

# Cluster label lookup ----
df_legend =
  comm_centers %>%
  arrange(desc(size)) %>%
  transmute(
    membership,
    size,
    cluster_label,
    legend_label = paste0(row_number(), ". ", cluster_label, " (n = ", size, ")")
  )

# --------------------------------------------------
# 2. Compute a local layout within each cluster
# --------------------------------------------------
cluster_ids = sort(unique(V(g_cluster)$membership))

node_positions = map_dfr(cluster_ids, function(this_cluster) {

  subg = induced_subgraph(
    g_cluster,
    vids = V(g_cluster)[V(g_cluster)$membership == this_cluster]
  )

  n_nodes = vcount(subg)

  # local layout within cluster
  if (n_nodes == 1) {
    local_xy = matrix(c(0, 0), ncol = 2)
  } else if (ecount(subg) == 0) {
    local_xy = layout_in_circle(subg)
  } else {
    local_xy = layout_with_fr(
      subg,
      weights = E(subg)$weight,
      niter = 2000
    )
  }

  local_df =
    tibble(
      name = V(subg)$name,
      membership = V(subg)$membership,
      x_local = local_xy[, 1],
      y_local = local_xy[, 2]
    )

  # rescale local coordinates so clusters have similar visual compactness
  if (n_nodes > 1) {
    local_df =
      local_df %>%
      mutate(
        x_local = rescale(x_local, to = c(-1, 1)),
        y_local = rescale(y_local, to = c(-1, 1))
      )
  } else {
    local_df =
      local_df %>%
      mutate(
        x_local = 0,
        y_local = 0
      )
  }

  # radius depends on cluster size
  this_size = vcount(subg)
  this_radius = 1.8 + 0.18 * sqrt(this_size)

  center =
    comm_centers %>%
    filter(membership == this_cluster)

  local_df %>%
    mutate(
      x = center$cx + x_local * this_radius,
      y = center$cy + y_local * this_radius
    )
})

# Actual cluster radius based on node positions
cluster_radius_df =
  node_positions %>%
  left_join(
    comm_centers %>% select(membership, cx, cy),
    by = "membership"
  ) %>%
  mutate(dist_to_center = sqrt((x - cx)^2 + (y - cy)^2)) %>%
  group_by(membership) %>%
  summarize(
    cluster_radius = max(dist_to_center, na.rm = TRUE),
    .groups = "drop"
  )

# Global center of all cluster centers
global_cx = mean(comm_centers$cx)
global_cy = mean(comm_centers$cy)

label_df =
  comm_centers %>%
  left_join(cluster_radius_df, by = "membership") %>%
  mutate(
    # direction pointing away from the middle of the full figure
    angle = atan2(cy - global_cy, cx - global_cx),

    # place label just outside the cluster boundary
    label_offset = cluster_radius + 1.8,
    label_x = cx + cos(angle) * label_offset,
    label_y = cy + sin(angle) * label_offset,

    label_text = str_wrap(
      paste0(cluster_label, "\n(n = ", size, ")"),
      width = 18
    )
  )

# Add coordinates back to graph
V(g_cluster)$x = node_positions$x[match(V(g_cluster)$name, node_positions$name)]
V(g_cluster)$y = node_positions$y[match(V(g_cluster)$name, node_positions$name)]

# --------------------------------------------------
# 3. Convert to tidygraph and add edge attributes
# --------------------------------------------------
V(g_cluster)$cluster_label =
  df_legend$legend_label[
    match(V(g_cluster)$membership, df_legend$membership)
  ]

g_cluster_tbl =
  as_tbl_graph(g_cluster) %>%
  activate(nodes) %>%
  mutate(
    membership = as.factor(membership),
    cluster_label = factor(
      cluster_label,
      levels = df_legend$legend_label
    )
  ) %>%
  activate(edges) %>%
  mutate(
    same_cluster = .N()$membership[from] == .N()$membership[to]
  )

# Color palette
# vec_color = c(
#   "#5B8FF9", "#61DDAA", "#65789B", "#F6BD16", "#7262FD",
#   "#78D3F8", "#9661BC", "#F6903D", "#008685", "#F08BB4"
# )

vec_color = c(
  "#4E79A7", "#59A14F", "#9C755F", "#E15759", "#B07AA1",
  "#EDC948", "#76B7B2", "#F28E2B", "#AF7AA1", "#2F4B7C"
)

# vec_color = c(
#   "#7C9885", "#69c672", "#B85C38", "#6C7A89", "#9A6FB0",
#   "#4C8C72", "#2e4746", "#3895d8", "#D8B26E", "#5B6C5D"
# )

color_map = setNames(vec_color, df_legend$legend_label)

# --------------------------------------------------
# 4. Plot
# --------------------------------------------------
p =
  ggraph(
    g_cluster_tbl,
    layout = "manual",
    x = V(g_cluster)$x,
    y = V(g_cluster)$y
  ) +
  geom_edge_link0(
    aes(filter = !same_cluster, width = weight),
    colour = "#000000",
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_edge_link0(
    aes(filter = same_cluster, width = weight),
    colour = "#000000",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(color = cluster_label),
    size = 2.2,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = color_map,
    name = "Policy document clusters"
  ) +
  scale_edge_width(range = c(0.15, 0.9)) +
  guides(
    color = guide_legend(
      ncol = 1,
      byrow = TRUE,
      override.aes = list(size = 6, alpha = 1)
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.02, 0.30),
    legend.justification = c(0, 0),
    legend.direction = "vertical",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.y = unit(0.5, "cm"),
    legend.background = element_rect(
      fill = alpha("white", 0.92),
      color = "grey40",
      linewidth = 0.4
    ),
    legend.key = element_rect(
      fill = alpha("white", 0),
      color = NA
    ), 
  ) 

p %>% ggsave(
  filename = "5-network/out/fig/top10_communities_network.jpg",
  width = 22,
  dpi = 300
)