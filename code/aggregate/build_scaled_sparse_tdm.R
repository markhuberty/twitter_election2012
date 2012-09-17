## 3 October 2011
## Code to build a new version of the DTM
## That works by summing the row-counts rather than
## concatenating tweets. Cat'ing tweets gives the
## problem of bigrams at the junction


library(Matrix)
library(foreach)

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

for(l in properties.list){

  print(l$purpose)
  print(l$scale.type)
  file.in <- paste("./data/doc_term_mat/generic.tdm.",
                   l$ngram,
                   ".",
                   l$purpose,
                   ".RData",
                   sep=""
                   )

  load(file.in)

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
  print(rownames(tdm.sparse))
  col.names <- colnames(tdm.sparse)
  row.names <- rownames(tdm.sparse)

  if(l$purpose=="topicmodel")
    {
      print("matrix constructed, converting to dtm")
      tdm.sparse <- sparse.to.dtm(tdm.sparse)
      print(tdm.sparse$dimnames)
    }

  filename <- paste("./data/doc_term_mat/tdm.sparse.",
                    l$ngram,
                    ".",
                    l$purpose,
                    ".",
                    l$agg,
                    ".",
                    l$scale.type,
                    ".RData",
                    sep=""
                    )
  
  print(dim(tdm.sparse))
  
  save(house.data,
       tdm.sparse,
       file=filename
       )
  
  rm(tdm.sparse)
  gc()

}
print("Aggregation complete")
