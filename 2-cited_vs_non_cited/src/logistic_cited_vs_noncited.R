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

mod_logistic = 
  glm(cited_num > 0 ~ log_citation_count + disruption + 
        log_newsfeed_count + log_tweet_count + 
        NCT_binary + NIH_binary + NSF_binary +
        doctype + 
        splines::ns(paper_age, df = 3) + 
        Field_Name.applied + Field_Name.biological + 
        Field_Name.clinical + Field_Name.developmental + 
        Field_Name.educational + Field_Name.experimental + Field_Name.mathematical + 
        Field_Name.multidisciplinary + Field_Name.psychoanalysis + Field_Name.other + 
        Field_Name.social, 
      data = df_SciSci_psych_papers_all %>% drop_na(), 
      family = binomial(link = "logit"))

summary(mod_logistic)
DescTools::PseudoR2(mod_logistic)
confint(mod_logistic) # Will take some time to run due to the large number of observations in the model.