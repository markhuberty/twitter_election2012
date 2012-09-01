## Generates predictions for the win/loss outcomes
library(SuperLearner)
library(foreach)
library(glmnet)
library(e1071)
library(randomForest)

source("./code/util/twitter.R")
load("./algorithms/binary.linear.predictor.RData")

filename <- "tdm.sparse.2.winloss.aggregate.scale.linear"
load(paste("./data/doc_term_mat/", filename, ".RData",  sep=""))

## Make sure that the input and output data are in the same order (st-dist)
## TODO Check that this is still right in the flow-through
corpus.district.tdm.mat <-
  tdm.sparse[order(rownames(tdm.sparse)),]

## Take out zero-valued rows
which.nonzero <- which(rowSums(as.matrix(corpus.district.tdm.mat)) > 0)

if(length(which.nonzero) > 0)
  corpus.district.tdm.mat <-  corpus.district.tdm.mat[which.nonzero, ]

## Retitle columns with generic names to handle a
## superlearner bug that hates spaces in colnames
orig.names <- colnames(corpus.district.tdm.mat)
new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
colnames(corpus.district.tdm.mat) <- new.names
rm(new.names)

## Scaled the data so columns are all mean=0, sd=1
corpus.district.tdm.mat.scale <- scale(corpus.district.tdm.mat)
corpus.district.tdm.mat.scale[is.nan(corpus.district.tdm.mat.scale)] <- 0

## Get the out-of-sample prediction accuracy estimates
SL.predict.discrete <- predict(train.superlearner.discrete,
                               newdata=corpus.district.tdm.mat.scale
                               )

outfile <- data.frame(rownames(corpus.district.tdm.mat),
                      SL.predict.discrete$fit,
                      as.integer(SL.predict.discrete$fit > 0.5)
                      )
names(outfile) <- c("state_district",
                    "prob.d.win",
                    "binary.d.win"
                    )
outfile$state_district <-
  as.character(outfile$state_district)


## Write out the csv data
today <- Sys.Date()

generic.outfile.name <-
  paste("./predictions/win_loss/binary.prediction.",
        today,
        ".csv",
        sep=""
                              )

master.outfile.name <- "./predictions/win_loss/binary.prediction.master.csv"
master.outfile.timestamp <-
  paste("./predictions/win_loss/binary.prediction.master",
        Sys.Date(),
        ".csv"
        )

## Save the generic prediction
write.csv(outfile,
          file=generic.outfile.name,
          row.names=FALSE
          )


## Load the master, concatenate, and save
outfile$prediction.date <- Sys.Date()
if(file.exists(master.outfile.name))
  {
    ## Append to the existing file with a date stamp
    master.csv <- read.csv(master.outfile.name,
                           stringsAsFactors=FALSE,
                           colClasses=c("character",
                             "numeric",
                             "integer",
                             "Date")
                           )
    master.outfile <- rbind(master.csv,
                            outfile
                            )
    write.csv(master.outfile,
              file=master.outfile.name,
              row.names=FALSE
              )
  }else{
    write.csv(outfile,
              file=master.outfile.name,
              row.names=FALSE
              )
  }
