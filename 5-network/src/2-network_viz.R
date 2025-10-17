
# Setting ----
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
# Load graph -----
g_poli_uni_largest = read_graph("5-network/data/df_psych_network_largest_comp_nodelist.graphml", format = "graphml")
## Network descriptives ----
degree(g_poli_uni_largest) %>% summary
degree(g_poli_uni_largest) %>% sd
E(g_poli_uni_largest)$weight %>% summary
E(g_poli_uni_largest)$weight %>% sd
# Community detection results ----
df_node = read_csv("5-network/out/python_sbm.csv")
vec_nodes = V(g_poli_uni_largest) %>% names()
# Reorder df to match the order of the nodes in the graph  
df_node = 
  df_node %>% 
  slice(match(vec_nodes, node))

# Map inferred class 
V(g_poli_uni_largest)$membership = df_node$community

temp = df_node %>% pull(community) %>% table %>% sort
length(temp) # 160 communities
temp_mem_num = 
  tibble(mem = names(temp), 
         num_poli = temp) %>% 
  arrange(desc(num_poli))

mean(temp_mem_num$num_poli)
sd(temp_mem_num$num_poli)

# Network viz -----
membership_vec = V(g_poli_uni_largest)$membership

## 0 --- Build community-level graph ----
edges_df = as_data_frame(g_poli_uni_largest, what = "edges") %>%
  mutate(
    from_comm = membership_vec[match(.data$from, V(g_poli_uni_largest)$name)],
    to_comm = membership_vec[match(.data$to, V(g_poli_uni_largest)$name)]
  ) %>%
  filter(from_comm != to_comm)  # only inter-community edges

edges_df = edges_df %>%
  rowwise() %>%
  mutate(
    c1 = min(from_comm, to_comm),
    c2 = max(from_comm, to_comm)
  ) %>%
  ungroup() %>%
  count(c1, c2, name = "weight")  # number of edges between communities

comm_sizes = temp_mem_num %>% as.data.frame()
colnames(comm_sizes) = c("community", "size")
comm_graph = graph_from_data_frame(
  d = edges_df,
  vertices = comm_sizes,
  directed = FALSE
)

## --- 1. Convert to tidygraph ---
top10_comm = temp_mem_num %>% head(10) %>% pull(mem) %>% as.character()
comm_tbl = 
  as_tbl_graph(comm_graph) %>% 
  mutate(
    size = as.numeric(size),
    name = as.character(name)
  )

## --- 2. Filter to top 10 communities and their connecting edges ---
vec_color = c(
    "#9cd3b9", "#dec77c", "#ff491b", "#7d93ff", "#c255ff",
    "#29bd00", "#bf007b", "#94e794", "#ffc466", "#32ecff"
)
comm_tbl_top10 = comm_tbl %>%
  activate(nodes) %>%
  filter(name %in% top10_comm) %>%
  activate(edges) %>%
  filter(.N()$name[from] %in% top10_comm & .N()$name[to] %in% top10_comm) %>%
  activate(nodes) %>%
  mutate(size = as.numeric(size)) %>% 
  mutate(color = vec_color)

## --- 3. Horizontally order top 10 communities (largest → smallest) ---
comm_order_top10 = comm_tbl_top10 %>%
  activate(nodes) %>%
  as_tibble() %>%
  distinct(name, size) %>%
  arrange(desc(size)) %>%
  mutate(
    rank = seq_len(n()),
    x = rank,   # left to right
    y = 0       # all aligned horizontally
  )

## --- 4. Assign coordinates to nodes ---
V(comm_tbl_top10)$x = comm_order_top10$x[match(V(comm_tbl_top10)$name, comm_order_top10$name)]
V(comm_tbl_top10)$y = comm_order_top10$y[match(V(comm_tbl_top10)$name, comm_order_top10$name)]

## --- 5. Build edge dataframe with coordinates and adjacency flag ---
edges_df = as_data_frame(comm_tbl_top10, what = "edges") %>%
  mutate(
    from_x = V(comm_tbl_top10)$x[match(.data$from, V(comm_tbl_top10)$name)],
    to_x   = V(comm_tbl_top10)$x[match(.data$to, V(comm_tbl_top10)$name)],
    from_y = V(comm_tbl_top10)$y[match(.data$from, V(comm_tbl_top10)$name)],
    to_y   = V(comm_tbl_top10)$y[match(.data$to, V(comm_tbl_top10)$name)],
    is_adjacent = abs(from_x - to_x) == 1
  )

edge_adj = edges_df %>% filter(is_adjacent)
edge_nonadj = edges_df %>% filter(!is_adjacent)

## --- 6. Initialize ggraph plot ---
p = ggraph(comm_tbl_top10, layout = "manual", x = V(comm_tbl_top10)$x, y = V(comm_tbl_top10)$y)

## --- 7. Add straight edges (adjacent) ---
if (nrow(edge_adj) > 0) {
  p = p +
    geom_edge_link0(
      data = edge_adj,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y, width = weight),
      color = "gray40",
      alpha = 0.7,
      lineend = "round"
    )
}

## --- 8. Add gently curved edges (non-adjacent) ---
if (nrow(edge_nonadj) > 0) {
  p = p +
    geom_curve(
      data = edge_nonadj,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y, size = weight),
      curvature = -0.25,
      color = "gray60",
      alpha = 0.3,
      lineend = "round"
    )
}

## --- 9. Add nodes (colored and labeled with size) ---
final_p =
  p +
    geom_node_point(
    aes(fill = I(color)),
    shape = 21,
    color = "black", # border color
    stroke = 0.6, 
    size = head(temp_mem_num$num_poli, 10)*.1, # Manually set sizes for better visibility
    alpha = .9
    ) +
    geom_node_text(
    aes(label = size),
    color = "black",
    size = 10,            # larger font
    vjust = 4,         # vertical center
    hjust = 0.5          # horizontal center
  ) + 
  scale_size(range = c(5, 14)) +
  scale_edge_width(range = c(0.3, 2)) +
  theme_void() + 
  theme(legend.position = "none")

final_p

ggsave(
  filename = "5-network/out/fig/top10_communities_network.jpg",  
  plot = final_p,                           
  width = 15,                               
  height = 6,                               
  dpi = 300                                 
)
