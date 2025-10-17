# Setting ----
library(tidyverse)
library(pscl)
library(rstatix)
library(arrow)

# Read in data -----
df_SciSci_psych_papers_all = read_csv("2-cited_vs_non_cited/data/SSN_all_psych_papers_field_cited_vs_noncited.csv")
# cited_num is the DV (cited by policy doucments)
# Citation_Count is the number of citations by other papers
# Other variables are self-explanatory

# Function to organize the hurdle regression results -----
get_sim_table = function(mod){
  sum_tab = summary(mod)[[1]]
  # Count part 
  count_tab = sum_tab[[1]]
  count_tab = cbind(count_tab, exp(count_tab[,1]))
  count_tab[,c(1:3, 5)] = round(count_tab[,c(1:3, 5)], 2)
  count_tab[,4] = round(count_tab[,4], 4)
  colnames(count_tab)[5] = "OR"
  count_tab = count_tab[, c(1,5,2:4)]
  # Hurdle part
  hurdle_tab = sum_tab[[2]]
  hurdle_tab = cbind(hurdle_tab, exp(hurdle_tab[,1]))
  hurdle_tab[,c(1:3, 5)] = round(hurdle_tab[,c(1:3, 5)], 2)
  hurdle_tab[,4] = round(hurdle_tab[,4], 4)
  colnames(hurdle_tab)[5] = "OR"
  hurdle_tab = hurdle_tab[, c(1,5,2:4)]
  return(list(hurdle_tab = hurdle_tab,  count_tab = count_tab))
}

get_coverage_rate = function(df){ 
  # Because rows with missing data will be removed in the model, we need to calculate the coverage rate based on the data used in the model
  df = df %>% drop_na()
  total_paper = nrow(df)
  n_cited = sum(df$cited_num > 0)
  n_non_cited = total_paper - n_cited

  coverage_n = list(total_paper = total_paper, 
                    n_cited = n_cited, 
                    n_non_cited = n_non_cited)
  coverage_rate = list(total_coverage = round(total_paper / total_paper_all, 4) * 100, 
                       coverage_cited = round(n_cited / n_cited_all, 4) * 100, 
                       coverage_non_cited = round(n_non_cited / n_non_cited_all, 4) * 100 
              )
  
  return(list(cover_all = c(coverage_n$total_paper, coverage_rate$total_coverage),
              cover_cited = c(coverage_n$n_cited, coverage_rate$coverage_cited),
              cover_non_cited = c(coverage_n$n_non_cited, coverage_rate$coverage_non_cited)))}

# Model prep ----- 
df_SciSci_psych_papers_all = df_SciSci_psych_papers_all %>% select(-PaperID)
total_paper_all = nrow(df_SciSci_psych_papers_all)
n_cited_all = sum(df_SciSci_psych_papers_all$cited_num > 0)
n_non_cited_all = total_paper_all - n_cited_all

# Hurdle model -----
# All the model include field fixed effects 
## Full hurdle model ----- 
# This could take a while to run
mod_hurdle_full_crt_fields = hurdle(cited_num ~ . | .,
                      data = df_SciSci_psych_papers_all %>% drop_na(),
                      dist = "negbin", 
                      control = hurdle.control(maxit = 100000, method = "BFGS"))
get_sim_table(mod_hurdle_full_crt_fields)
cov_mod_hurdle_full_crt = get_coverage_rate(df_SciSci_psych_papers_all %>% drop_na())
cov_mod_hurdle_full_crt # Coverage rate was good 

## Bivariate correlation ----
cor_mat = 
  df_SciSci_psych_papers_all %>% 
  select(cited_num, Citation_Count, Disruption, 
         Newsfeed_Count, Tweet_Count, 
         NCT_binary, NIH_binary, NSF_binary) %>%
  cor_mat() 

cor_mat %>% cor_mark_significant() # Low correlation among the predictors --> no multicollinearity concerns