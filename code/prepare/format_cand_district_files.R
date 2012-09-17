## Code to generate a _wide_ version of the candidate file
## for use on the visualizations
## Expects a candidates file in "long" format (one row per state +
## district + party). Generates a "wide" format with one row per state_district.

library(reshape)
library(plyr)
library(gdata)

candidates <- read.csv("./data/candidates.csv",
                       stringsAsFactors=FALSE)

party.district.count <- table(candidates$state_dist, candidates$party)
unopposed <- which(apply(party.district.count, 1, function(x) any(x == 0)))


## Generate and write out the set of contested districts
candidates.twoparty <- candidates[!(candidates$state_dist %in% names(unopposed)),]
write.csv(candidates.twoparty,
          file="./data/candidates.final.2012.csv",
          row.names=FALSE
          )

## Subset the names for what we need in the district file
cand.to.merge <- candidates.twoparty[,c("state_dist",
                                        "state_id",
                                        "district",
                                        "name",
                                        "party",
                                        "incumbent",
                                        "first_name",
                                        "last_name"
                                        )
                                     ]                              
cand.to.merge$incumbent <- as.character(cand.to.merge$incumbent)

## Reshape the file into wide format
cand.melt <- melt(cand.to.merge,
                  id.vars=c("state_dist",
                    "state_id",
                    "district",
                    "party"
                    )
                  )
cand.cast <- cast(cand.melt,
                  state_dist + state_id + district~
                  party * variable
                  )

## Set the incumbent party appropriately
cand.cast$incumbent_party <-
  ifelse(as.logical(cand.cast$R_incumbent), "R",
         ifelse(as.logical(cand.cast$D_incumbent), "D",
                "O"
                )
         )

## Dump the party-specific incumbency indicators
cand.cast <- cand.cast[,!(names(cand.cast) %in% c("R_incumbent",
                                                  "D_incumbent"
                                                  )
                          )
                       ]

## Write out and exit
write.csv(cand.cast, file="./data/candidates_wide.csv", row.names=FALSE)
          
