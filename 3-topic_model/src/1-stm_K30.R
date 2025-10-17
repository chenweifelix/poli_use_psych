rm(list = ls())
gc()
library(tidyverse)
library(stm)

# Read in data ----- 
stm_processed = read_rds("3-topic_model/data/processed_text_for_stm.rds")

set.seed(1234) # Set seed for reproducibility
# Fit model -------
# This will take a very long time to run
# I ran this on a high-performance computing cluster and specified 24 cores with 30G memory
# It took me about 5 hours to run 
sink("3-topic_model/out/stm/txt_out/transform_PrevFit_K30.txt") 
print("Fitting PrevFit with K=30")
PrevFit = 
    stm(documents = stm_processed$documents, vocab = stm_processed$vocab, K = 30, 
        prevalence = ~cited_poli_bin+z_log_cited_aca_num+Field_Name.applied+Field_Name.biological+
                      Field_Name.clinical+Field_Name.developmental+Field_Name.educational+
                      Field_Name.experimental+Field_Name.mathematical+Field_Name.multidisciplinary+
                      Field_Name.other+Field_Name.social, 
        max.em.its = 75, data = stm_processed$meta, init.type = "Spectral")
sink()
write_rds(PrevFit, "3-topic_model/out/stm/transform_PrevFit_K30.rds")
