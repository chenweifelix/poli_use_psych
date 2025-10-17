# Load Library ----- 
library(tidyverse)
# Read in data ------ 
df_poli_and_papers_fields = read_csv("1-trend/data/df_poli_and_papers_fields.csv") # NA in Field_name means the cited paper is from a non-social science field 

# Count policy as citing a field, regardless of the number of paper the policy cites ----- 
# Trend across years (2000-2022) ---- 
df_n_poli_year = 
  df_poli_and_papers_fields %>% 
  group_by(published_on_yr) %>% 
  summarize(n_poli_year = n_distinct(policy_document_id))

fields_acorss_years_policy_cite = 
  df_poli_and_papers_fields %>% 
  select(policy_document_id, published_on_yr, Field_name) %>% 
  group_by(published_on_yr, Field_name) %>% 
  summarize(count = n_distinct(policy_document_id)) %>% 
  mutate(year = factor(published_on_yr)) %>% 
  left_join(df_n_poli_year, by = "published_on_yr") %>% 
  mutate(percentage = count/n_poli_year)

fields_acorss_years_policy_cite = 
  fields_acorss_years_policy_cite %>% 
  mutate(Field_name = str_to_title(Field_name)) %>% 
  arrange(desc(percentage))

fields_acorss_years_policy_cite_mean = 
  fields_acorss_years_policy_cite %>% ungroup() %>% 
  group_by(Field_name) %>% 
  summarize(mean_percent = mean(percentage) * 100) %>% 
  arrange(desc(mean_percent))

## Plot 
legend_order = fields_acorss_years_policy_cite_mean %>% ungroup() %>% distinct(Field_name) %>% pull(Field_name)
legend_order = c(legend_order[2], legend_order[3], legend_order[c(4,6:18,5,1)])
color = c("#f2f3fc",pals::stepped2()[2:20])
color = c("gray70",pals::stepped2()[2:18])
year_color = c("gray70", pals::stepped2())
plt_all_fields_percent_m1 = 
  fields_acorss_years_policy_cite %>% 
  ungroup %>% 
  mutate(Field_Name = factor(Field_name, levels = rev(legend_order)), 
        year = as.numeric(year)) %>% 
  filter(Field_Name != "Other Unlabeled Fields") %>% 
  ggplot(aes(x = Field_Name, y = percentage)) + 
  geom_point(aes(color = year)) + 
  gghalves::geom_half_boxplot(side = "r", nudge = .3, outlier.alpha = 0) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black", position = position_nudge(x = .2)) + 
  xlab(" ") + ylab(" ") + 
  scale_colour_gradientn(colours = viridis::inferno(n = 21), name = " ", 
                         labels = function(x) as.character(x + 1999), 
                         breaks = c(1, 6, 11, 16, 21)) + 
  scale_y_continuous(breaks=seq(0,1,.05), labels = function(x) paste0(x * 100, "%"), position = "right", expand = c(0.009,0.009)) + 
  ggpubr::theme_pubclean() +
  theme(axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.position = "bottom",
        legend.key = element_rect(fill = NA), 
        legend.key.width = unit(5, units = "cm") 
  ) + 
  guides(colour = guide_colourbar(label = T, draw.ulim = T, draw.llim = T)) +
  coord_flip()
plt_all_fields_percent_m1
ggsave(plt_all_fields_percent_m1, filename = "1-trend/out/plt_all_fields_percent.pdf", 
       width = 16, height = 8)

### Only showing the top 5 fields 
plt_all_fields_percent_m1_top5 =   
  fields_acorss_years_policy_cite %>% 
  ungroup %>% 
  mutate(Field_Name = factor(Field_name, levels = rev(legend_order)), 
        year = as.numeric(year)) %>% 
  filter(Field_Name != "Other Unlabeled Fields") %>% 
  filter(Field_Name %in% head(legend_order, 5)) %>% # The top 5
  ggplot(aes(x = Field_Name, y = percentage)) + 
  geom_point(aes(color = year), size = 2.5) + 
  gghalves::geom_half_boxplot(side = "r", nudge = .3, outlier.alpha = 0, errorbar.draw = F) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black", position = position_nudge(x = .2), size = 1, linewidth = 1) + 
  xlab(" ") + ylab(" ") + 
  scale_colour_gradientn(colours = viridis::inferno(n = 21), name = " ", 
                         labels = function(x) as.character(x + 1999), 
                         breaks = c(1, 6, 11, 16, 21)) + 
  scale_y_continuous(breaks=seq(0,1,.05), labels = function(x) paste0(x * 100, "%"), position = "right", expand = c(0.009,0.009)) + 
  ggpubr::theme_pubclean() +
  theme(axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.position = "bottom",
        legend.key = element_rect(fill = NA), 
        legend.key.width = unit(5, units = "cm") 
  ) + 
  guides(colour = guide_colourbar(label = T, draw.ulim = T, draw.llim = T)) +
  coord_flip()
plt_all_fields_percent_m1_top5
ggsave(plt_all_fields_percent_m1_top5, filename = "1-trend/out/plt_all_fields_percent_m1_top5.pdf", 
       width = 16, height = 8)

## Ranking -----
df_rank_across_years = 
  fields_acorss_years_policy_cite %>% 
  filter(!is.na(Field_name)) %>%
  arrange(published_on_yr, desc(percentage)) %>% 
  mutate(ranking = row_number()) 

# Ranked 2 in 19 years, and 3 in 2 years 
df_rank_across_years %>% 
  filter(Field_name == "Psychology") %>% pull(ranking) %>% table

# Range of percentage across years ----
df_rank_across_years %>% 
  filter(Field_name == "Psychology") %>% pull(percentage) %>% range %>% 
  round(4) * 100
# Mean and sd of percentage across years ----
df_rank_across_years %>% 
  filter(Field_name == "Psychology") %>% ungroup %>% 
  summarize(mean = mean(percentage)*100, sd = sd(percentage)*100) 

# Percentage growth ----
df_all_mod_of_time = 
  fields_acorss_years_policy_cite %>% 
  filter(Field_name != "Other Unlabeled Fields") %>%
  mutate(year = as.numeric(year), percentage = percentage * 100) %>% 
  group_by(Field_name) %>% 
  do(tidy(lm(percentage ~ year, .))) %>% 
  filter(term != "(Intercept)") %>% 
  ungroup() %>% 
  mutate(p_value_adj = stats::p.adjust(p.value, method = "bonferroni") %>% round(4)) %>% 
  mutate(sig = ifelse(p_value_adj < .001, "***", ifelse(p_value_adj < .01, "**", ifelse(p_value_adj < .05, "*", "")))) %>% 
  arrange(desc(estimate))
df_all_mod_of_time

## Test sig difference among the trends of fields ------
func_test_coef = function(b1, se1, b2, se2){
  z = (b1 - b2) / sqrt( (se1*b1)^2 + (se2*b2)^2 )
  p = 2 * (1 - pnorm(abs(z)))
  return(list(z,p))
}

# Compare with the second largest trend (Political Science)
func_test_coef(df_all_mod_of_time[[which(df_all_mod_of_time$Field_name == "Psychology"), "estimate"]], 
               df_all_mod_of_time[[which(df_all_mod_of_time$Field_name == "Psychology"), "std.error"]], 
               df_all_mod_of_time[[2, "estimate"]], 
               df_all_mod_of_time[[2, "std.error"]]) # Not sig diff
# Compare with the third largest trend (Economics)
func_test_coef(df_all_mod_of_time[[which(df_all_mod_of_time$Field_name == "Psychology"), "estimate"]], 
               df_all_mod_of_time[[which(df_all_mod_of_time$Field_name == "Psychology"), "std.error"]], 
               df_all_mod_of_time[[3, "estimate"]], 
               df_all_mod_of_time[[3, "std.error"]]) # Sig diff
# Compare with the fourth largest trend (Education)
func_test_coef(df_all_mod_of_time[[which(df_all_mod_of_time$Field_name == "Psychology"), "estimate"]], 
               df_all_mod_of_time[[which(df_all_mod_of_time$Field_name == "Psychology"), "std.error"]], 
               df_all_mod_of_time[[4, "estimate"]], 
               df_all_mod_of_time[[4, "std.error"]]) # Sig diff

# Other fields will be sig diff from psychology (smaller trend)
