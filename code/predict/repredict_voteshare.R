## Generates predictions for the win/loss outcomes
library(SuperLearner)
library(foreach)
library(glmnet)
library(e1071)
library(randomForest)
library(reshape)

setwd("/mnt/fwire_80/twitter_election2012/")
source("./code/util/twitter.R")
load("./algorithms/voteshare.linear.predictor.RData")

date.seq <- seq.Date(as.Date("2012-09-17"),
                     as.Date("2012-11-06"),
                     "day"
                     )
date.seq <- as.character(date.seq)

first.time <- TRUE
for(d in date.seq){

  filename <- "tdm.sparse.2.voteshare.aggregate.scale.uniform."
  load(paste("./data/doc_term_mat/rebuild/",
             filename,
             d,
             ".RData",
             sep="")
       )

  ## Make sure that the input and output data are in the same order (st-dist)
  corpus.district.tdm.mat <-
    tdm.sparse[order(rownames(tdm.sparse)),]

  which.nonzero <- which(rowSums(as.matrix(corpus.district.tdm.mat)) > 0)

  if(length(which.nonzero) > 0)
    corpus.district.tdm.mat <-  corpus.district.tdm.mat[which.nonzero, ]


  ## Retitle columns with generic names to handle a
  ## superlearner bug that hates spaces in colnames

  ## First sort the columns by the name vector used to generate the
  ## predictor
  corpus.district.tdm.mat <- corpus.district.tdm.mat[,orig.names]
  
  ## Then replace the names with placeholders to keep the learner happy
  orig.names <- colnames(corpus.district.tdm.mat)
  new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
  colnames(corpus.district.tdm.mat) <- new.names
  rm(new.names)

  ## Scaled the data so rows sum to 1
  corpus.district.tdm.mat.scale <-
    corpus.district.tdm.mat / rowSums(corpus.district.tdm.mat)
  corpus.district.tdm.mat.scale <- as.matrix(corpus.district.tdm.mat.scale)
  corpus.district.tdm.mat.scale[is.nan(corpus.district.tdm.mat.scale)] <- 0

  ## Get the out-of-sample prediction accuracy estimates
  ## NOTE: for this to work, requires arm package
  ## version 1.3.07. Otherwise errors ensue.
  SL.predict.cont <- predict(train.superlearner.cont,
                             newdata=corpus.district.tdm.mat.scale
                             )

  print("Prediction successful, generating output")
  outfile <- data.frame(rownames(corpus.district.tdm.mat),
                        SL.predict.cont$fit
                        )

  names(outfile) <- c("state_district",
                      "dem_vote_share"
                      )

  outfile$state_district <-
    as.character(outfile$state_district)

  outfile$prediction.date <- d

  if(first.time)
    {

      master.prediction <- outfile
      first.time <- FALSE
      
    }else{

      master.prediction <- rbind(master.prediction, outfile)
      
    }
  ## Write out the csv data
  today <- d

  generic.outfile.name <-
    paste("./predictions/vote_share/repredict/continuous.prediction.",
          today,
          ".csv",
          sep=""
          )

  master.outfile.name <-
    "./predictions/vote_share/repredict/continuous.prediction.master.csv"

  print("Saving voteshare prediction output")
  ## Save the generic prediction
  write.csv(outfile,
            file=generic.outfile.name,
            row.names=FALSE
            )

  write.csv(master.prediction,
            file=master.outfile.name,
            row.names=FALSE
            )

}
