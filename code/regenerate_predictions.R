
library(SuperLearner)
library(glmnet)
library(e1071)
library(randomForest)
library(reshape)


source("./code/util/build_sparse_functions.R")
source("./code/util/twitter.R")

## scale here equiv to either 0 + 1x (linear)
## or 1 + x^2

scale.type <- c("scale.uniform", "scale.linear")
purposes <- c("voteshare",
              "winloss",
              "topicmodel")
type <- c("aggregate", "byweek")

## Generate the necessary params
voteshare.agg.params <- list(purpose="voteshare",
                             ngram=2,
                             agg="aggregate",
                             scale=TRUE,
                             scale.type="scale.uniform",
                             scale.params=NULL,
                             initial.threshold=NULL,
                             final.threshold=NULL,
                             tfidf.threshold=NULL,
                             tfidf.threshold=NULL,
                             tfidf.filter=FALSE,
                             sparse.filter=FALSE
                             )
winloss.agg.params <- list(purpose="winloss",
                           ngram=2,
                           agg="aggregate",
                           scale=TRUE,
                           scale.type="scale.linear",
                           scale.params=c(1,0),
                           initial.threshold=NULL,
                           final.threshold=NULL,
                           tfidf.threshold=NULL,
                           tfidf.filter=FALSE,
                           sparse.filter=FALSE
                           )
voteshare.byweek.params <- list(purpose="voteshare",
                                ngram=2,
                                agg="byweek",
                                scale=TRUE,
                                scale.type="scale.uniform",
                                scale.params=NULL,
                                initial.threshold=NULL,
                                final.threshold=NULL,
                                tfidf.threshold=NULL,
                                tfidf.threshold=NULL,
                                tfidf.filter=FALSE,
                                sparse.filter=FALSE
                                )
winloss.byweek.params <- list(purpose="winloss",
                              ngram=2,
                              agg="byweek",
                              scale=TRUE,
                              scale.type="scale.linear",
                              scale.params=c(1,0),
                              initial.threshold=NULL,
                              final.threshold=NULL,
                              tfidf.threshold=NULL,
                              tfidf.filter=FALSE,
                              sparse.filter=FALSE
                              )
topicmodel.params <- list(purpose="topicmodel",
                          ngram=1,
                          agg="aggregate",
                          scale=TRUE,
                          scale.type="scale.uniform",
                          scale.params=NULL,
                          initial.threshold=4,
                          final.threshold=0.01,
                          tfidf.threshold=0.001,
                          tfidf.filter=TRUE,
                          sparse.filter=TRUE
                          )

properties.list <- list(voteshare.agg.params,
                        winloss.agg.params,
                        voteshare.byweek.params,
                        winloss.byweek.params,
                        topicmodel.params
                        )

voteshare.files <-
  system("ls ./data/doc_term_mat | grep generic | grep voteshare| grep 2012",
         intern=TRUE
         )

winloss.files <-
  system("ls ./data/doc_term_mat | grep generic | grep winloss| grep 2012",
         intern=TRUE
         )

## Do the winloss predictions first
predict.new <- TRUE
winloss.prediction <- NULL
winloss.model <- load("./algorithms/binary.linear.predictor.RData")
for(f in winloss.files){
  l <- winloss.agg.params
  file.date <- gsub("generic.tdm.2.winloss.", "", f)
  file.date <- gsub(".RData", "", file.date)
  ## First aggregate
  load(paste("./data/doc_term_mat/",f, sep=""))

  col.names <- unlist(tdm.corpus$dimnames[2])

  if(l$agg == "aggregate")
    {
      agg.fac <- house.data$state_dist
    }else if(l$agg == "byweek"){
      agg.fac <- paste(house.data$state_dist,
                       house.data$tweet.age,
                       sep="."
                       )
    }

  time.var <- as.Date(house.data$created_at)
  if(l$purpose=="topicmodel")
    {
      ## Only model the last 5 days worth of stuff
      period.min <- Sys.Date() - 5
      in.period <- time.var >= period.min
      agg.fac <- agg.fac[in.period]
      tdm.corpus <- tdm.corpus[in.period]
      time.var <- time.var[in.period]
    }

  tdm.sparse <-
    generate.sparse.tdm(tdm.corpus,
                        agg.fac=agg.fac,
                        initial.threshold=l$initial.threshold,
                        final.threshold=l$final.threshold,
                        col.names=col.names,
                        scale=l$scale,
                        scale.fun=l$scale.type,
                        scale.params=l$scale.params,
                        time.var=time.var,
                        tfidf.filter=l$tfidf.filter,
                        tfidf.threshold=l$tfidf.threshold,
                        sparse.filter=l$sparse.filter
                        )
  ## print(rownames(tdm.sparse))
  col.names <- colnames(tdm.sparse)
  row.names <- rownames(tdm.sparse)

  at.large.names <- row.names %in% c("AK01",
                                     "DE01",
                                     "MT01",
                                     "ND01",
                                     "SD01"
                                     )
  rownames(tdm.sparse)[at.large.names] <-
    gsub("01", "00",
         rownames(tdm.sparse)[at.large.names]
         )

  tdm.filename <- paste("./data/doc_term_mat/test/winloss_tdm_",
                        file.date,
                        ".RData",
                        sep=""
                        )
  save(tdm.sparse, house.data,
       file=tdm.filename
       )


  ## Then predict
  ## Make sure that the input and output data are in the same order (st-dist)
  ## TODO Check that this is still right in the flow-through
  if(predict.new)
    {
      corpus.district.tdm.mat <-
        tdm.sparse[order(rownames(tdm.sparse)),]

      ## Take out zero-valued rows
      which.nonzero <- which(rowSums(as.matrix(corpus.district.tdm.mat)) > 0)

      if(length(which.nonzero) > 0)
        corpus.district.tdm.mat <-  corpus.district.tdm.mat[which.nonzero, ]

      ## Retitle columns with generic names to handle a
      ## superlearner bug that hates spaces in colnames
      corpus.district.tdm.mat <- corpus.district.tdm.mat[,orig.names]
      new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
      colnames(corpus.district.tdm.mat) <- new.names
      rm(new.names)

      ## Scaled the data so rows sum to 1
      corpus.district.tdm.mat.scale <-
        corpus.district.tdm.mat / rowSums(corpus.district.tdm.mat)
      corpus.district.tdm.mat.scale <- as.matrix(corpus.district.tdm.mat.scale)
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

      out.name <- paste("./predictions/win_loss/binary.prediction.",
                        file.date,
                        ".csv",
                        sep=""
                        )
      write.csv(outfile, file=out.name, row.names=FALSE)

      outfile$prediction.date <- file.date


      if(is.null(winloss.prediction))
        {
          winloss.prediction <- outfile
        }else{
          winloss.prediction <- rbind(winloss.prediction,
                                      outfile
                                      )
        }
    }
}


winloss.melt <- melt(winloss.prediction,
                     id.vars=c("state_district", "prediction.date")
                     )
winloss.wide <- cast(winloss.melt,
                     state_district ~ prediction.date + variable
                     )

write.csv(winloss.prediction,
          file="./predictions/win_loss/binary.prediction.master.csv",
          row.names=FALSE
          )

write.csv(winloss.wide,
          file="./predictions/win_loss/binary.prediction.master.wide.csv",
          row.names=FALSE
          )



## Then do the same for the voteshare data
voteshare.prediction <- NULL
voteshare.model <- load("./algorithms/voteshare.linear.predictor.RData")

for(f in voteshare.files){
  l <- voteshare.agg.params
  file.date <- gsub("generic.tdm.2.voteshare.", "", f)
  file.date <- gsub(".RData", "", file.date)
  ## First aggregate
  load(paste("./data/doc_term_mat/",f, sep=""))

  col.names <- unlist(tdm.corpus$dimnames[2])

  if(l$agg == "aggregate")
    {
      agg.fac <- house.data$state_dist
    }else if(l$agg == "byweek"){
      agg.fac <- paste(house.data$state_dist,
                       house.data$tweet.age,
                       sep="."
                       )
    }

  time.var <- as.Date(house.data$created_at)
  if(l$purpose=="topicmodel")
    {
      ## Only model the last 5 days worth of stuff
      period.min <- Sys.Date() - 5
      in.period <- time.var >= period.min
      agg.fac <- agg.fac[in.period]
      tdm.corpus <- tdm.corpus[in.period]
      time.var <- time.var[in.period]
    }

  tdm.sparse <-
    generate.sparse.tdm(tdm.corpus,
                        agg.fac=agg.fac,
                        initial.threshold=l$initial.threshold,
                        final.threshold=l$final.threshold,
                        col.names=col.names,
                        scale=l$scale,
                        scale.fun=l$scale.type,
                        scale.params=l$scale.params,
                        time.var=time.var,
                        tfidf.filter=l$tfidf.filter,
                        tfidf.threshold=l$tfidf.threshold,
                        sparse.filter=l$sparse.filter
                        )
  ## print(rownames(tdm.sparse))
  col.names <- colnames(tdm.sparse)
  row.names <- rownames(tdm.sparse)

  at.large.names <- row.names %in% c("AK01",
                                     "DE01",
                                     "MT01",
                                     "ND01",
                                     "SD01"
                                     )
  rownames(tdm.sparse)[at.large.names] <-
    gsub("01", "00",
         rownames(tdm.sparse)[at.large.names]
         )

  tdm.filename <- paste("./data/doc_term_mat/test/voteshare_tdm_",
                        file.date,
                        ".RData",
                        sep=""
                        )
  save(tdm.sparse, house.data,
       file=tdm.filename
       )


  ## Then predict
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
  corpus.district.tdm.mat <- corpus.district.tdm.mat[,orig.names]
  orig.names <- colnames(corpus.district.tdm.mat)
  new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
  colnames(corpus.district.tdm.mat) <- new.names
  rm(new.names)

  ## Scaled the data so rows sum to 1
  corpus.district.tdm.mat.scale <-
    corpus.district.tdm.mat / rowSums(corpus.district.tdm.mat)
  corpus.district.tdm.mat.scale <- as.matrix(corpus.district.tdm.mat.scale)
  corpus.district.tdm.mat.scale[is.nan(corpus.district.tdm.mat.scale)] <- 0

  if(predict.new)
    {
      ## Get the out-of-sample prediction accuracy estimates
      SL.predict.cont <- predict(train.superlearner.cont,
                                 newdata=corpus.district.tdm.mat.scale
                                 )

      outfile <- data.frame(rownames(corpus.district.tdm.mat),
                            SL.predict.cont$fit
                            )
      names(outfile) <- c("state_district",
                          "dem_vote_share"
                          )
      outfile$state_district <-
        as.character(outfile$state_district)

      out.name <- paste("./predictions/vote_share/continuous.prediction.",
                        file.date,
                        ".csv",
                        sep=""
                        )
      write.csv(outfile, file=out.name, row.names=FALSE)

      outfile$prediction.date <- file.date

      if(is.null(voteshare.prediction))
        {
          voteshare.prediction <- outfile
        }else{
          voteshare.prediction <- rbind(voteshare.prediction,
                                        outfile
                                        )
        }

    }
}


write.csv(voteshare.prediction,
          file="./predictions/vote_share/continuous.prediction.master.csv",
          row.names=FALSE
          )

voteshare.melt <- melt(voteshare.prediction,
                       id.vars=c("state_district", "prediction.date")
                       )
voteshare.wide <- cast(voteshare.melt,
                       state_district ~ prediction.date + variable
                       )

write.csv(voteshare.wide,
          file="./predictions/vote_share/continuous.prediction.master.wide.csv",
          row.names=FALSE
          )
