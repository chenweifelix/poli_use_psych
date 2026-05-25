# Load Library ----- 
library(tidyverse)
library(ggpubr)
library(tidytext)

dotchat_text_size = 18
dotchat_title_size = 20

# Policy categories ---- 
## OVT ----
df_poli_ovt_cat = read_csv("4-poli_cate/data/df_poli_ovt_cat.csv")

df_cat_percent_each_field = 
  df_poli_ovt_cat %>% 
  group_by(classifications) %>% # Get the number of policy documents in each category
  mutate(n_poli_in_a_cat = n_distinct(policy_document_id)) %>% 
  group_by(classifications, Field_name) %>% 
  summarize(n_poli_citing_a_field = n_distinct(policy_document_id), 
            n_poli_in_a_cat = mean(n_poli_in_a_cat)) %>% 
  mutate(percentage_field = (n_poli_citing_a_field/n_poli_in_a_cat) * 100)

df_cat_percent_each_field %>% 
  filter(Field_name == "psychology") %>% 
  arrange(desc(percentage_field)) %>% 
  filter(percentage_field >= 10) 

df_bar_plot = 
  df_cat_percent_each_field %>% 
  filter(Field_name == "psychology") %>% 
  arrange(percentage_field) %>% 
  ungroup %>% 
  mutate(percentage_counter = 100 - percentage_field, order = row_number()) %>% 
  pivot_longer(cols = c(percentage_field, percentage_counter), 
               names_to = "type", 
               values_to = "percentage") 
vec_issues = df_bar_plot %>% distinct(classifications) %>% pull(1)
vec_all_poli_per_cat = df_cat_percent_each_field %>% 
  filter(Field_name == "psychology") %>% 
  arrange(percentage_field) %>% pull(n_poli_in_a_cat)

plt_poli_cat = 
  df_bar_plot %>% 
  ggplot(aes(x = order, y = percentage)) +
  geom_point(size = 4) +
  geom_bar(aes(fill = type), stat = "identity") +
  geom_label(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5), size = 5, color = "black") +
  scale_fill_manual(values = c("percentage_field" = "#ffa41c", "percentage_counter" = "lightgrey"), 
                    labels = rev(c("Policy documents citing psychology", "Policy documents not citing psychology"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(
    breaks = 1:length(vec_issues), 
    labels = str_to_title(vec_issues) %>% str_replace_all("And", "&"),
    sec.axis = sec_axis(~.,
                            breaks = 1:length(vec_all_poli_per_cat),
                            labels = vec_all_poli_per_cat %>% scales::comma())) +
  ylab("Percentage of policy documents citing psychology papers") + xlab(" ") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(size = dotchat_text_size),
        plot.title = element_text(size = dotchat_title_size),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "top")
 
plt_poli_cat %>% ggsave(filename = "4-poli_cate/out/plt_poli_cat.pdf", width = 16, height = 8)

## SGD ---- 
df_poli_sdg_cat = read_csv("4-poli_cate/data/df_poli_ovt_sdg.csv")

df_fields_sgd = 
  df_poli_sdg_cat %>% 
  group_by(sdgcategories) %>% 
  mutate(n_poli_in_a_cat = n_distinct(policy_document_id)) %>% 
  group_by(sdgcategories, Field_name) %>% 
  summarize(n_poli_citing_a_field = n_distinct(policy_document_id), n_poli_in_a_cat = mean(n_poli_in_a_cat)) %>% 
  mutate(percentage_field = (n_poli_citing_a_field/n_poli_in_a_cat) * 100)

df_fields_sgd %>% 
  filter(Field_name == "psychology") %>% 
  filter(!is.na(sdgcategories)) %>% 
  arrange(desc(percentage_field)) %>% 
  filter(percentage_field >= 10) 

df_bar_plot_sgd = 
  df_fields_sgd %>%
  filter(Field_name == "psychology") %>% 
  filter(!is.na(sdgcategories)) %>% 
  arrange(percentage_field) %>% 
  ungroup %>% 
  mutate(percentage_counter = 100 - percentage_field, order = row_number()) %>% 
  pivot_longer(cols = c(percentage_field, percentage_counter), 
               names_to = "type", 
               values_to = "percentage")
vec_issues_sgd = df_bar_plot_sgd %>% distinct(sdgcategories) %>% pull(1)
vec_all_poli_per_cat_sgd = df_fields_sgd %>% 
  filter(Field_name == "psychology") %>% 
  filter(!is.na(sdgcategories)) %>% 
  arrange(percentage_field) %>% pull(n_poli_in_a_cat)
plt_poli_cat_sgd = 
  df_bar_plot_sgd %>% 
  ggplot(aes(x = order, y = percentage)) +
  geom_point(size = 4) +
  geom_bar(aes(fill = type), stat = "identity") +
  geom_label(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5), size = 5, color = "black") +
  scale_fill_manual(values = c("percentage_field" = "#1ca0ff", "percentage_counter" = "lightgrey"), 
                    labels = rev(c("Policy documents citing psychology", "Policy documents not citing psychology"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(
    breaks = 1:length(vec_issues_sgd), 
    labels = vec_issues_sgd,
    sec.axis = sec_axis(~.,
                        breaks = 1:length(vec_all_poli_per_cat_sgd),
                        labels = vec_all_poli_per_cat_sgd %>% scales::comma())) +
  ylab("Percentage of policy documents citing psychology papers") + xlab(" ") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(size = dotchat_text_size),
        plot.title = element_text(size = dotchat_title_size),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "top")
plt_poli_cat_sgd
plt_poli_cat_sgd %>% ggsave(filename = "4-poli_cate/out/plt_poli_cat_sgd.pdf", width = 16, height = 8)
