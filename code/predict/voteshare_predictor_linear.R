## Generates predictions for the win/loss outcomes
library(SuperLearner)
library(foreach)
library(glmnet)
library(e1071)
library(randomForest)
library(reshape)

source("./code/util/twitter.R")
load("./algorithms/voteshare.linear.predictor.RData")

filename <- "tdm.sparse.2.voteshare.aggregate.scale.uniform"
load(paste("./data/doc_term_mat/", filename, ".RData",  sep=""))

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

## Write out the csv data
today <- Sys.Date()

generic.outfile.name <-
  paste("./predictions/vote_share/continuous.prediction.",
        today,
        ".csv",
        sep=""
                              )

master.outfile.name <-
  "./predictions/vote_share/continuous.prediction.master.csv"
master.outfile.wide.name <-
  "./predictions/vote_share/continuous.prediction.master.wide.csv"
master.outfile.timestamp <-
  paste("./predictions/vote_share/continuous.prediction.master",
        Sys.Date(),
        ".csv"
        )

print("Saving voteshare prediction output")
## Save the generic prediction
write.csv(outfile,
          file=generic.outfile.name,
          row.names=FALSE
          )

## Write out a JSON version of the same
json.name <- gsub("csv", "json", generic.outfile.name)
prediction.results.toJSON(outfile$state_district,
                          outfile$dem_vote_share,
                          json.name
                          )

## Add the filename to the list of voteshare outputs
sink("./predictions/vote_share/voteshare_filenames.txt", append=TRUE)
cat(gsub("./predictions/vote_share/", "", json.name))
cat("\n")
sink()


## Load the master, concatenate, and save
outfile$prediction.date <- Sys.Date()
if(file.exists(master.outfile.name))
  {
    ## Append to the existing file with a date stamp
    master.csv <- read.csv(master.outfile.name,
                           stringsAsFactors=FALSE,
                           colClasses=c("character",
                             "numeric",
                             "Date")
                           )

    ## Discard prior predictions for this date
    master.csv <-
      master.csv[master.csv$prediction.date != Sys.Date(), ]

    master.outfile <- rbind(master.csv,
                            outfile
                            )
    master.outfile.melt <- melt(master.outfile,
                                id.vars=c("state_district", "prediction.date")
                                )
    master.outfile.wide <- cast(master.outfile.melt,
                                state_district ~ prediction.date,
                                fun.aggregate="mean",
                                na.rm=TRUE
                                )

    write.csv(master.outfile,
              file=master.outfile.name,
              row.names=FALSE
              )
    write.csv(master.outfile.wide,
              file=master.outfile.wide.name,
              row.names=FALSE
              )
  }else{
    master.outfile.melt <- melt(outfile,
                                id.vars=c("state_district", "prediction.date"),
                                fun.aggregate="mean",
                                na.rm=TRUE
                                )
    master.outfile.wide <- cast(master.outfile.melt,
                                state_district ~ prediction.date,
                                fun.aggregate="mean",
                                na.rm=TRUE
                                )

    write.csv(outfile,
              file=master.outfile.name,
              row.names=FALSE
              )
    write.csv(master.outfile.wide,
              file=master.outfile.wide.name,
              row.names=FALSE
              )
  }

## Generate ratings based on voteshare
voteshare.ratings <- compute.rating(master.outfile.wide,
                                    voteshare=TRUE,
                                    cutpoint.intervals=c(0, 45, 49,
                                      51, 55, 100),
                                    labels=NULL,
                                    n.periods=5,
                                    label.type="integer"
                                    )

rating.filename <- paste("./predictions/vote_share/voteshare_rating_",
                         today,
                         ".json"
                         )
prediction.results.toJSON(master.outfile.wide$state_district,
                          voteshare.ratings,
                          rating.filename
                          )
