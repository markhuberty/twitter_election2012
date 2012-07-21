## Code to get and parse the NYT election results
## 3 November 2010

library(XML)

urls <- c("http://elections.nytimes.com/2010/results/house/big-board",
          "http://elections.nytimes.com/2010/results/senate/big-board"
          )

results.house <- readHTMLTable(urls[1])
results.senate <- readHTMLTable(urls[2])

results.house <- results.house[-1]
results.senate <- results.senate[-1]

library(plyr)

results.house.df <- ldply(results.house, data.frame)
results.senate.df <- ldply(results.senate, data.frame)

names(results.house.df)  <- c("state",
                              "district",
                              "pctDem",
                              "pctRep",
                              "pctReport"
                              )

names(results.senate.df) <-  c("state",
                               "district",
                               "pctDem",
                               "pctRep",
                               "pctInd",
                               "pctReport"
                               )


## Use regexp to grep out and reformat the state/district information
results.house.df$pctDem <- as.numeric(gsub("%",
                                           "",
                                           as.character(results.house.df$pctDem),
                                           fixed=TRUE
                                           )
                                      )
results.house.df$pctRep <- as.numeric(gsub("%",
                                           "",
                                           as.character(results.house.df$pctRep),
                                           fixed=TRUE
                                           )
                                      )


results.house.df$pctReport <- as.numeric(gsub("%",
                                              "",
                                              as.character(results.house.df$pctReport),
                                              fixed=TRUE
                                              )
                                         )

results.senate.df$pctDem <- as.numeric(gsub("%",
                                            "",
                                           as.character(results.senate.df$pctDem),
                                           fixed=TRUE
                                           )
                                      )
results.senate.df$pctRep <- as.numeric(gsub("%",
                                            "",
                                           as.character(results.senate.df$pctRep),
                                           fixed=TRUE
                                           )
                                      )

results.senate.df$pctInd <- as.numeric(gsub("%",
                                            "",
                                           as.character(results.senate.df$pctInd),
                                           fixed=TRUE
                                           )
                                      )
results.senate.df$pctReport <- as.numeric(gsub("%",
                                               "",
                                              as.character(results.senate.df$pctReport),
                                              fixed=TRUE
                                              )
                                         )



                                                        

results.house.df$state <- gsub("([A-Za-z]*)\\.{0,1}\\s[0-9]*",
                               "\\1",
                               as.character(results.house.df$district)
                               )
results.house.df$state <- gsub(".", "", results.house.df$state,
                               fixed=TRUE)

results.senate.df$state <- gsub("([A-Za-z]*)\\.{0,1}",
                               "\\1",
                               as.character(results.senate.df$district)
                               )

results.house.df$district.num <- as.integer(
                                            gsub("[A-Za-z.]*\\s([0-9]*)",
                                                 "\\1",
                                                 as.character(results.house.df$district)
                                                 )
                                            )
                                      
                                                   

## Code district for senator number. Note NYC has a special election
results.senate.df$district <- c(1,1,1,2,rep(1,
                                            dim(results.senate.df)[1]-4)
                                )

results.senate.df$unc <- is.na(results.senate.df$pctReport)
results.house.df$unc <- is.na(results.house.df$pctReport)

state.codes <- read.csv("states_nytimes.csv")

results.senate.df$state <- tolower(results.senate.df$state)
results.house.df$state <- tolower(results.house.df$state)

## Fix the nyc special election
results.senate.df$state[grepl("ny",
                              as.character(results.senate.df$state))
                        ] <- "ny"

results.house.df <- merge(results.house.df,
                          state.codes,
                          by.x="state",
                          by.y="nytState"
                          )

results.senate.df <- merge(results.senate.df,
                           state.codes,
                           by.x="state",
                           by.y="nytState",
                           all.x=TRUE
                           )

fips <- read.csv("fipscodes.csv", header=TRUE)

results.house.df <- merge(results.house.df,
                          fips,
                          by.x="st.abbr",
                          by.y="USA",
                          all.x=TRUE
                          )

results.senate.df <- merge(results.senate.df,
                           fips,
                           by.x="st.abbr",
                           by.y="USA",
                           all.x=TRUE
                           )

results.house.df$d.victory <- ifelse(results.house.df$pctDem >=50,
                                     1,
                                     0)
results.house.df$d.victory <- ifelse(results.house.df$pctRep <50,
                                     1,
                                     results.house.df$d.victory
                                     )


## NOTE: files were edited by hand after writing out, to deal
## with by-district idiosyncracies. If you re-run this code,
## you must re-jigger the outputs (i.e. on uncontested race outcomes)
write.table(results.house.df, file="results_house_2010.csv", sep=",",
            row.names=FALSE
            )
write.table(results.senate.df, file="results_senate_2010.csv", sep=",",
            row.names=FALSE
            )

rm(list=ls())

results.house <- read.csv("results_house_2010.csv", header=TRUE)
results.senate <- read.csv("results_senate_2010.csv", header=TRUE)

house.dem <- house.rep <- results.house

house.dem <- house.dem[,c(1:4,6:9,11)]
house.rep <- house.rep[,c(1:3,5:9,11)]


house.dem$party <- "D"
house.rep$party <- "R"

names(house.dem) <- names(house.rep) <- c("st.abbr", "state",
                                          "district", "pctVote",
                                          "pctReport", "dist.num",
                                          "unc", "state.long",
                                          "d.victory", "party"
                                          )


results.house <- rbind(house.dem,
                       house.rep
                       )

write.table(results.house, file="results_house_2010.csv", sep=",",
            row.names=FALSE)



senate.dem <- results.senate[,c(1:4,7:9, 11:12)]
senate.rep <- results.senate[,c(1:3, 5,7:9, 11:12)]
senate.ind <- results.senate[,c(1:3,6:9, 11:12)]

senate.dem$party <- "D"
senate.rep$party <- "R"
senate.ind$party <- "I"

names(senate.dem) <- names(senate.rep) <- names(senate.ind) <- c("st.abbr",
                                                                 "state",
                                                                 "district",
                                                                 "pctVote",
                                                                 "pctReport",
                                                                 "unc",
                                                                 "state.long",
                                                                 "d.victory",
                                                                 "r.victory",
                                                                 "party"
                                                                 )
                                            
results.senate <- rbind(senate.dem, senate.rep, senate.ind)

results.senate.twoparty <- results.senate[results.senate$party!="I",]

write.table(results.senate, file="results_senate_2010.csv", sep=",")
write.table(results.senate.twoparty, file="results_senate_twoparty_2010.csv", sep=",")

