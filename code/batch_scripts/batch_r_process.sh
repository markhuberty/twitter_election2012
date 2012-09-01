#!/usr/local/bin/Rscript --verbose

#setwd("~/projects/twitter_election2012")
setwd("/mnt/fwire_80/twitter_election2012")

print("Acquiring data")
source("./code/acquire/cron_search_job.R")

print("Building doc-term matrices")
source("./code/clean/build_doc_term_mat.R")

print("Aggregating doc-term matrices")
source("./code/aggregate/build_scaled_sparse_tdm.R")

print("Generating voteshare predictions")
source("./code/predict/voteshare_predictor_linear.R")

print("Generating win-loss predictions")
source("./code/predict/winloss_predictor_linear.R")

print("Modeling topics")
source("./code/model/topicmodel_district.R")

print("Done")


