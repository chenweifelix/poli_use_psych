# Load Library ----- 
library(tidyverse)
library(ggpubr)
library(tidytext)

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
