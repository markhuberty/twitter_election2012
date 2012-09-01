## 3 October 2011
## Code to build a new version of the DTM
## That works by summing the row-counts rather than
## concatenating tweets. Cat'ing tweets gives the
## problem of bigrams at the junction


setwd("~/Documents/twitter_election2012")


library(Matrix)
library(foreach)

source("./code/util/build_sparse_functions.R")

## scale here equiv to either 0 + 1x (linear)
## or 1 + x^2
scale.params <- list(NULL, c(1, 0), c(1, 0, 1, 2), c(0.08))
scale.type <- c("scale.uniform", "scale.linear")
purposes <- c("voteshare", "winloss")
type <- c("aggregate", "byweek")

ngrams <- c(1,2)
for(p in purposes){
  for(i in ngrams){

    file.in <- paste("./data/doc_term_mat/generic.tdm.",
                     i,
                     ".",
                     p,
                     ".RData",
                     sep=""
                     )

    load(file.in)


    agg.fac.wk <- paste(house.data$state_dist,
                        house.data$tweet.age,
                        sep="."
                        )

    agg.fac.dist <- house.data$state_dist


    ## Don't aggregate by week if scaling, doesn't make sense
    agg.fac.list <- list(agg.fac.dist,
                         agg.fac.wk
                         )
    scale.agg <- c(TRUE, FALSE)


    col.names <- unlist(tdm.corpus$dimnames[2])

    for(j in 1:length(agg.fac.list))
      { ## Agg levels
        for(k in 1:length(scale.type))
          { ## scaling types
            tdm.sparse <-
              generate.sparse.tdm(tdm.corpus,
                                  agg.fac=agg.fac.list[[j]],
                                  initial.threshold=NULL,
                                  final.threshold=NULL,
                                  col.names=col.names,
                                  scale=scale.agg[j],
                                  time.var=as.Date(house.data$created_at),
                                  scale.fun=scale.type[k],
                                  scale.params=scale.params[[k]],
                                  sparse.filter=FALSE
                                  )

            filename <- paste("./data/doc_term_mat/tdm.sparse.",
                              i,
                              ".",
                              p,
                              ".",
                              type[j],
                              ".",
                              scale.type[k],
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
      }
    rm(house.data, tdm.corpus)
    gc()
  }
}
