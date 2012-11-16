## 13 November 2012
## Mark Huberty
## Script to generate cumulative master files to study why some races
## changed so much after mid-October

library(Matrix)
library(foreach)
library(tm)
library(RWeka)

setwd("/mnt/fwire_80/twitter_election2012")
source("./code/util/build_sparse_functions.R")
source("./code/util/twitter.R")

## Load up the master file for the voteshare data

## Clean and aggregate the data; want to generate for each day
## starting 10-15 and ending on 11-6
time.format <- "%Y-%m-%d"

min.date <- as.Date(strptime("2012-10-02", time.format))
max.date <- as.Date(strptime("2012-11-06", time.format))

date.seq <- seq.Date(from=min.date, to=max.date, by="day")

tdm.sparse.byday <- list()
for(i in 1:length(date.seq)){

  d <- date.seq[i]
  file.to.load <- paste("./data/doc_term_mat/generic.tdm.master.2.voteshare.",
                        as.character(d),
                        ".RData",
                        sep=""
                        )
  print(d)
  load(file.to.load)
  
  ## Generate the sparse matrix version and aggregate
  col.names <- unlist(tdm.corpus$dimnames[2])
  time.var <- as.Date(house.data$created_at)
  agg.fac <- house.data$state_dist
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

  tdm.sparse <-
    generate.sparse.tdm(tdm.corpus,
                        agg.fac=agg.fac,
                        initial.threshold=voteshare.agg.params$initial.threshold,
                        final.threshold=voteshare.agg.params$final.threshold,
                        col.names=col.names,
                        scale=voteshare.agg.params$scale,
                        scale.fun=voteshare.agg.params$scale.type,
                        scale.params=voteshare.agg.params$scale.params,
                        time.var=time.var,
                        tfidf.filter=voteshare.agg.params$tfidf.filter,
                        tfidf.threshold=voteshare.agg.params$tfidf.threshold,
                        sparse.filter=voteshare.agg.params$sparse.filter
                        )
  ## print(rownames(tdm.sparse))
  col.names <- colnames(tdm.sparse)
  row.names <- rownames(tdm.sparse)


  
  out <- list(tdm.sparse, house.data, col.names, row.names)
  tdm.sparse.byday[[length(tdm.sparse.byday) + 1]] <- out


}
names(tdm.sparse.byday) <- date.seq

save(tdm.sparse.byday, file="./data/doc_term_mat/tdm.sparse.byday.RData")
