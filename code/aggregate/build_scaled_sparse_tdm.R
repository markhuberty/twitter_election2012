## 3 October 2011
## Code to build a new version of the DTM
## That works by summing the row-counts rather than
## concatenating tweets. Cat'ing tweets gives the
## problem of bigrams at the junction


setwd("~/Documents/twitter_election2012")  


library(Matrix)
library(foreach)

source("./code/util/build_sparse_functions.R")

initial.threshold <- c(3,3) 
final.threshold <- c(0.02, 0.005)

## scale here equiv to either 0 + 1x (linear)
## or 1 + x^2
scale.params <- list(c(1, 0), c(1, 0, 1, 2), c(0.08))
scale.type <- "scale.linear"

type <- c("aggregate", "byweek")

    i <- 2
    ## 2 code for unigrams/bigrams/trigrams
    file.in <- "./data/generic.tdm.2.RData"
    
    load(file.in)

    
    agg.fac.wk <- paste(house.data$state,
                        house.data$district,
                        house.data$tweet.age,
                        sep="."
                        )

    agg.fac.dist <- paste(house.data$state,
                          house.data$district,
                          sep="."
                          )


    ## Don't aggregate by week if scaling, doesn't make sense
    agg.fac.list <- list(agg.fac.dist## ,
                         ## agg.fac.wk 
                         )


    col.names <- unlist(tdm.corpus$dimnames[2])

    for(j in 1:length(agg.fac.list))
      { ## Agg levels
        for(k in 1:length(scale.type))
          {
            tdm.sparse <-
              generate.sparse.tdm(tdm.corpus,
                                  agg.fac=agg.fac.list[[j]],
                                  initial.threshold=initial.threshold[j],
                                  final.threshold=final.threshold[j],
                                  col.names=col.names,
                                  scale=TRUE,
                                  time.var=house.data$created_at,
                                  scale.fun=scale.type[k],
                                  scale.params=scale.params[[k]]
                                  )

            filename <- paste("./data/tdm.sparse.",
                              i,
                                        #".pctvote.unigrams.",
                              final.threshold[j],
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
  
