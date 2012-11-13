#!/usr/local/bin/Rscript --verbose

#setwd("~/projects/twitter_election2012")
setwd("/mnt/fwire_80/twitter_election2012")

print("Acquiring data")
source("./code/acquire/cron_search_job.R")
rm(list=ls())
gc()

print("Building doc-term matrices")
source("./code/clean/build_doc_term_mat.R")
rm(list=ls())
gc()

# print("Aggregating doc-term matrices")
# source("./code/aggregate/build_scaled_sparse_tdm.R")
# rm(list=ls())
# gc()

# print("Generating voteshare predictions")
# source("./code/predict/voteshare_predictor_linear.R")
# rm(list=ls())
# gc()

# print("Generating win-loss predictions")
# source("./code/predict/winloss_predictor_linear.R")
# rm(list=ls())
# gc()

# print("Modeling topics")
# source("./code/model/topicmodel_district.R")
# rm(list=ls())
# gc()

print("Done")


