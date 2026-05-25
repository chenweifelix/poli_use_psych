# Setting ------
gc()
rm(list = ls())
library(stm)
library(tidyverse)
library(ggpubr)
# Read in original data ----- 
df_papers_abs_info = arrow::read_feather("3-topic_model/data/cited_paper_info.feather")

# Read in precessed text -----
out = read_rds("3-topic_model/data/processed_text_lower_threashold.rds")

# Model fit in Quest ------- 
PrevFit_k30 = read_rds("3-topic_model/out/stm/transform_PrevFit_K30.rds")
PrevFit_k30$settings$covariates$formula
# Avg Proportion of each 
mat_prop = PrevFit_k30$theta

obj_topic_words = labelTopics(PrevFit_k30)

# This only needs to be run once to save time
# Check if file exists
mod_out_filepath = "3-topic_model/out/stm/mod_eff_k30.rds"
set.seed(1234)
if (!file.exists(mod_out_filepath)) {
    prep_mod = estimateEffect(1:30 ~ 
                            cited_poli_bin + z_log_cited_aca_num + 
                            Field_Name.applied+Field_Name.biological+
                            Field_Name.clinical+Field_Name.developmental+Field_Name.educational+
                            Field_Name.experimental+Field_Name.mathematical+Field_Name.multidisciplinary+
                            Field_Name.other+Field_Name.social, 
                            PrevFit_k30, 
                            meta = out$meta, 
                            uncertainty = "Global")
    saveRDS(prep_mod, mod_out_filepath)
} else {
    prep_mod = read_rds(mod_out_filepath)
}

vec_sum_tab = summary(prep_mod)

mat_result = matrix(nrow = 3, ncol = 3); colnames(mat_result) = c("aca_pos", "aca_neg", "aca_ns");rownames(mat_result) = c("poli_pos", "poli_neg", "poli_ns")
vec_pos_pos = c(); vec_pos_neg = c(); vec_pos_ns = c(); 
vec_neg_pos = c(); vec_neg_neg = c(); vec_neg_ns = c();
vec_ns_pos = c(); vec_ns_neg = c(); vec_ns_ns = c();
vec_poli_eff_all = c(); vec_poli_se_all = c() 
vec_aca_eff_all = c(); vec_aca_se_all = c()
vec_topic_and_words_all = c()
vec_keyword_freq = c(); vec_keyword_frex = c(); vec_keyword_lift = c(); vec_keyword_score = c()
for (i_topic in vec_sum_tab$topics){
    cat("Examining topic: ", i_topic, "\n")
    vec_freq = obj_topic_words$prob[i_topic, ] # High probability words --> The most frequent words in that topic.
    vec_keyword_freq = c(vec_keyword_freq, str_c(vec_freq, collapse = ", "))
    vec_frex = obj_topic_words$frex[i_topic, ] # High FREX words --> Captures distinctive and salient words—often the most interpretable.
    vec_keyword_frex = c(vec_keyword_frex, str_c(vec_frex, collapse = ", "))
    vec_lift = obj_topic_words$lift[i_topic, ] # High lift words --> Highlights words that are overrepresented in a topic relative to their overall frequency.
    vec_keyword_lift = c(vec_keyword_lift, str_c(vec_lift, collapse = ", "))
    vec_score = obj_topic_words$score[i_topic, ] # High score words --> Emphasizes words that are statistically distinctive.
    vec_keyword_score = c(vec_keyword_score, str_c(vec_score, collapse = ", "))

    vec_keywords = vec_freq # Use high frequency words to label topics; change for other types of words

    # Get the effect size and significance
    tab_temp = vec_sum_tab$tables[i_topic][[1]]
    poli_sig = tab_temp[2,4]; aca_sig = tab_temp[3,4]
    poli_eff = tab_temp[2,1]; aca_eff = tab_temp[3,1]
    poli_eff = tab_temp[2,1]; aca_eff = tab_temp[3,1]
    poli_se = tab_temp[2,2]; aca_se = tab_temp[3,2]
    vec_poli_eff_all = c(vec_poli_eff_all, poli_eff) 
    vec_poli_se_all = c(vec_poli_se_all, poli_se)
    vec_aca_eff_all = c(vec_aca_eff_all, aca_eff)
    vec_aca_se_all = c(vec_aca_se_all, aca_se)

    prop_top = (mat_prop[, i_topic] %>% mean() %>% round(4)) * 100 
    vec_temp = str_c("Topic ", i_topic, " (", prop_top, "%)", ": " ,str_c(vec_keywords, collapse = ", "), "\n")
    vec_topic_and_words_all = c(vec_topic_and_words_all, vec_temp)

    if (poli_sig < .05 & aca_sig < .05) { # If significant in both
        if (poli_eff >=0 & aca_eff >= 0) { 
            vec_pos_pos = str_c(vec_pos_pos, vec_temp)
        } else if (poli_eff >=0 & aca_eff < 0) {
            vec_pos_neg = str_c(vec_pos_neg, vec_temp)
        } else if(poli_eff < 0 & aca_eff < 0){
            vec_neg_neg = str_c(vec_neg_neg, vec_temp)
        } else if (poli_eff < 0 & aca_eff >= 0) {
            vec_neg_pos = str_c(vec_neg_pos, vec_temp)
        } 
    } else if (poli_sig < .05 & aca_sig >= .05) {
      if (poli_eff >=0) {
          vec_pos_ns = str_c(vec_pos_ns, vec_temp)
          } else {
          vec_neg_ns = str_c(vec_neg_ns, vec_temp)}
    } else if (poli_sig >= .05 & aca_sig < .05) {
        if (aca_eff >= 0) {
            vec_ns_pos = str_c(vec_ns_pos, vec_temp)
        } else {
            vec_ns_neg = str_c(vec_ns_neg, vec_temp)}
    } else {
        vec_ns_ns = str_c(vec_ns_ns, vec_temp)
    }
}
for (vec in c("vec_pos_pos", "vec_pos_neg", "vec_pos_ns", "vec_neg_pos", "vec_neg_neg", "vec_neg_ns", "vec_ns_pos", "vec_ns_neg", "vec_ns_ns")){
    if (is.null(get(vec))) {
        assign(vec, c(" "))
    }
}

# All topic coeficients plot -----
df_eff_plt = 
    tibble(poli_eff = vec_poli_eff_all, poli_se = vec_poli_se_all, 
           aca_eff = vec_aca_eff_all, aca_se = vec_aca_se_all) %>% 
    mutate(poli_low_ci = poli_eff - 1.96 * poli_se, poli_high_ci = poli_eff + 1.96 * poli_se,
           aca_low_ci = aca_eff - 1.96 * aca_se, aca_high_ci = aca_eff + 1.96 * aca_se) %>% 
    mutate(across(everything() , ~round(. * 100 , 2))) 

temp_poli = df_eff_plt %>% select(poli_eff, poli_low_ci, poli_high_ci)    
colnames(temp_poli)[1:3] = c("eff", "low_ci", "high_ci")
temp_aca = df_eff_plt %>% select(aca_eff, aca_low_ci, aca_high_ci)
colnames(temp_aca)[1:3] = c("eff", "low_ci", "high_ci")

df_eff_plt_long = 
    bind_rows(
        temp_poli %>% mutate(type = "Policy"), 
        temp_aca %>% mutate(type = "Academic")) %>%
    mutate(topic = rep(vec_topic_and_words_all %>% str_trim(), 2)) %>% 
    separate(topic, into = c("topic", "top_words"), sep = ": ") 

# Get represenative abstracts for each topic
get_high_theta_docs = function(out, theta_mat, topic, threshold = 0.5, top_n = 50) {
  theta_topic = theta_mat[, topic]
  
  idx = which(theta_topic > threshold)
  
  if (length(idx) == 0) {
    return(character(0))
  }
  
  idx = idx[order(theta_topic[idx], decreasing = TRUE)]
  idx = head(idx, top_n)
  
  paper_vec = out$meta[idx, "str_title_abs"]
  return(paper_vec)
}

list_abs = list()
for (i in 1:30){
    paper_vec = get_high_theta_docs(out, mat_prop, i, threshold = 0.5, top_n = 50)
    topic_label = str_c("Topic ", i)
    if (length(paper_vec) > 50){ # Limit to the top 50 abstracts 
        paper_vec = head(paper_vec, 50)
    }
    # paper_vec = paper_vec %>% str_c(collapse = "\n\n---\n\n") 
    list_abs[[topic_label]] = paper_vec
}


# Use llm to name the topics (done in computing cluster)
df_topic_name_desc = read_csv("3-topic_model/out/topic_name_desc.csv")
vec_topic_names = df_topic_name_desc$topic_name

df_eff_plt_long = df_eff_plt_long %>% 
    mutate(topic_name = rep(vec_topic_names, 2)) %>% 
    mutate(topic_name = str_to_title(topic_name) %>% str_remove("\\.$")) %>% 
    mutate(topic_name = topic_name %>% str_replace_all("And", "&")) %>% 
    mutate(topic_axis = str_replace(topic, "Topic ", "#") %>% str_replace(" \\(", "; ")) %>% 
    mutate(topic_axis = str_c("(", topic_axis)) %>% 
    mutate(topic_axis = str_c(topic_name, " ", topic_axis))
df_eff_plt_policy = df_eff_plt_long %>% filter(type == "Policy") # Use this to set the order
df_eff_plt_policy = df_eff_plt_policy %>% arrange(eff, topic)
y_axis_topic = df_eff_plt_policy$topic_axis 
y_axis_top_words = df_eff_plt_policy$top_words
df_eff_plt_long = df_eff_plt_long %>% 
    mutate(topic_axis = factor(topic_axis, levels = y_axis_topic))

fig_eff_plot = 
    df_eff_plt_long %>% 
    ggplot(aes(y = as.numeric(topic_axis), color = type, shape = type)) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    geom_linerange(aes(xmin = low_ci, xmax = high_ci), size = 2) + 
    geom_point(aes(x = eff), size=10) +
    theme_minimal() + 
    labs(y = " ", color = "Type", shape = "Type") + 
    scale_color_manual(values = c("Policy" = "#f9b222", "Academic" = "#c785fab7")) + 
    scale_y_continuous(
        breaks = 1:length(y_axis_topic),
        labels = y_axis_topic,
        sec.axis = sec_axis(~.,
                            breaks = 1:length(y_axis_top_words),
                            labels = y_axis_top_words)) + 
    scale_x_continuous(breaks = seq(-1.5, 1.5, .5), 
                       limits = c(-1.5, 1.5), 
                       name = "Effect of policy document citation (%)",
                       sec.axis = sec_axis(~., 
                            breaks = seq(-1, 1.5, .5), 
                            name = "Effect of paper citation (%)"

                       )) +
    theme(axis.title = element_text(size = 30), 
          axis.text = element_text(size = 30), 
          legend.title = element_blank(), 
          legend.text = element_text(size = 30), 
          legend.position = "top", 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          axis.title.x = element_text(margin = margin(t = 20)),
          axis.title.x.top = element_text(margin = margin(b = 20))
          )

fig_eff_plot

ggsave(fig_eff_plot, filename = "3-topic_model/out/fig/fig_eff_plt.jpeg", width = 35, height = 20, dpi = 500)

# Table for SI -----
vec_sig_poli = c()
vec_sig_aca = c()
for (i in 1:30){
    tab_temp = vec_sum_tab$tables[i][[1]]
    poli_sig = tab_temp[2,4]; aca_sig = tab_temp[3,4]
    vec_sig_poli = c(vec_sig_poli, poli_sig)
    vec_sig_aca = c(vec_sig_aca, aca_sig)
}
vec_sig_all = c(vec_sig_poli, vec_sig_aca) 

eff_tab_SI = 
    df_eff_plt_long %>% 
    select(eff, low_ci, high_ci, type, topic_axis, topic) %>% 
    mutate(sig = vec_sig_all) %>% 
    mutate(sig_mark = case_when(
        sig < .001 ~ "***",
        sig < .01 ~ "**",
        sig < .05 ~ "*",
        TRUE ~ "ns"
    ))  

eff_tab_SI 
