## Begun 22 Jan 2011
## Purpose:
## Concatenate the by-candidate aggregate tweet corpus
## to a district-level corpus for all two-candidate districts
## Then predict party outcome/vote
setwd("~/Documents/twitter_election2012")

library(SuperLearner)
library(ggplot2)
library(foreach)
library(glmnet)
library(e1071)
library(randomForest)

source("./code/twitter.R")
source("./code/build_sparse_functions.R")


filename <- "tdm.sparse.20.01.aggregate.scale.linear"
load(paste("./data/", filename, ".RData",  sep=""))
#corpus.district.tdm.mat <- select.tfidf(tdm.sparse, threshold=0.0005)
rm(tdm.sparse)
gc()

## Make sure that the input and output data are in the same order (st-dist)
## TODO Check that this is still right in the flow-through
corpus.district.tdm.mat <-
  corpus.district.tdm.mat[order(rownames(corpus.district.tdm.mat)),]

## Retitle columns with generic names
orig.names <- colnames(corpus.district.tdm.mat)
new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
colnames(corpus.district.tdm.mat) <- new.names
rm(new.names)

## Scaled the data so columns are all mean=0, sd=1
corpus.district.tdm.mat.scale <- scale(corpus.district.tdm.mat)

## Get the out-of-sample prediction accuracy estimates
SL.predict.discrete <- predict(train.superlearner.discrete,
                               newdata=corpus.district.tdm.mat
                               )

outfile <- data.frame(cand.data.dist$state,
                      cand.data.dist$district,
                      cand.data.dist$party,
                      cand.data.dist$first.name,
                      cand.data.dist$last.name,
                      SL.predict.discrete
                      as.integer(SL.predict.discrete)
                      )


## Write out the csv data
today <- Sys.Date()

generic.outfile.name <- paste("binary.prediction.",
                              today,
                              ".csv",
                              sep=""
                              )
                             
master.outfile.name <- "binary.prediction.master.csv"
master.outfile.timestamp <- paste("binary.prediction.master",
                                  Sys.Date(),
                                  ".csv"
                                  )

## Save the generic prediction
write.csv(outfile,
          file=generic.outfile.name,
          row.names=FALSE
          )


## Load the master, concatenate, and save
master.csv <- read.csv(master.outfile.name, headers=TRUE)
outfile$date <- Sys.Date()
master.outfile <- rbind(master.csv,
                        outfile
                        )
write.csv(master.outfile,
          file=master.outfile.name
          row.names=FALSE
          )

## Note this isn't quite right yet. Want to generate:
## 1. the single file
## 2. a master file
## 3. a date-stamped master so we can have it for posterity
## And needs to write out as a csv.
