## 
## Code to build a new version of the DTM
## That works by summing the row-counts rather than
## concatenating tweets. Cat'ing tweets gives the
## problem of bigrams at the junction

setwd("~/Documents/twitter_election2012")

library(tm)
library(RWeka)
library(foreach)

source("./code/util/twitter.R")

load("./data/twitter.data.house.results.RData")

## Make sure we are only looking at D/R candidates
d.r <- twitter.data.house.results$party %in% c("D", "R")
house.data <- twitter.data.house.results[d.r,]
rm(d.r, twitter.data.house.results)

## Make sure that all tweets come from before the election
election.date <- as.Date("2012-11-6", format="%Y-%m-%d")
before.date <- house.data$created.at.date < election.date
house.data <- house.data[before.date, ]

tweet.age <- round(as.numeric(difftime(election.date,
                                       house.data$created.at.date,
                                       units="weeks"
                                       )
                              ),
                   0
                   )
tweet.age <- max(tweet.age) - tweet.age

house.data$tweet.age <- tweet.age
rm(tweet.age)

## Then make sure we are only looking at races with challengers.
idx.chal <- find.chal.2(house.data$state,
                        house.data$district,
                        house.data$party,
                        idx=FALSE
                        )

has.chal <- paste(idx.chal[,1],
                  idx.chal[,2],
                  sep="."
                  )
st.dist <- paste(house.data$state,
                 house.data$district,
                 sep="."
                 )

house.data <- house.data[st.dist %in% has.chal,]
rm(idx.chal, st.dist, has.chal)
gc()

house.data <- house.data[,c("state",
                            "district",
                            "party",
                            "first.name.x",
                            "last.name.x",
                            "created.at.date",
                            "from_user_id",
                            "to_user_id",
                            "text",
                            "pctVote",
                            "d.victory",
                            "tweet.age",
                            "chal.idx"
                            )
                         ]

names(house.data) <- c("state",
                       "district",
                       "party",
                       "first.name",
                       "last.name",
                       "created.at.date",
                       "from.user.id",
                       "to.user.id",
                       "text",
                       "pctVote",
                       "d.victory",
                       "tweet.age",
                       "chal.idx"
                       )


## Now clear out the junk in the tweets themselves
corpus <- tolower(house.data$text)

## Hack out the New Orleans Saints data
which.nola <-
  grepl("kicker", corpus) | grepl("orleans", corpus) |
  grepl("tampa bay",corpus) |
grepl("saints", corpus) |
grepl("hartley", corpus) |
grepl("cliff lee", corpus)

corpus <- corpus[!which.nola]
house.data <- house.data[!which.nola, ]


## First remove all misc cruft
corpus <- remove.all(corpus)

## Then replace the major political figures
corpus <- replace.president(corpus)
corpus <- replace.pol.names(corpus,
                            house.data$state,
                            house.data$district,
                            st.speaker="OH",
                            st.leader="NV",
                            dist.speaker=8
                            )

## Then replace the candidate names with dummies
corpus <- replace.candidate(house.data,
                            corpus
                            )

## Need to get the actual challenger index
chal.idx <- find.chal.idx(house.data$state,
                          house.data$district,
                          house.data$party
                          )

house.data$chal.idx[chal.idx[,1]] <- chal.idx[,2]
rm(chal.idx)
gc()

## Replace the opponent names with dummies
corpus <- replace.opponent.2(corpus,
                              house.data$first.name,
                              house.data$last.name,
                              house.data$chal.idx
                              )

## Then replace the cand/opp dummies with
## party-specific variants

corpus <- party.dummies(corpus,
                        house.data$party
                        )

corpus <- Corpus(VectorSource(corpus),
                 readerControl=list(readPlain),
                 language="en",
                 load=TRUE
                 )

## Strip out stopwords, whitespace, punctuation
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)

## Then turn into a doc-term matrix

save(corpus,
     file="./data/generic.corpus.RData"
     )

max.terms <- 3
for(i in 1:max.terms)
  {

    my.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = i,
                                                               max = i)
                                               )


    tdm.corpus <- DocumentTermMatrix(corpus,
                                     control=list(tokenize=my.tokenizer,
                                       weighting=weightTf)
                                     )

    ## Generate the date-stamped file
    today <- Sys.Date()
    timestamp.file.name <- paste("./data/generic.tdm.",
                                 i,
                                 ".",
                                 today,
                                 sep=""
                                 )

    ## Generate the generic file
    generic.file.name <- paste("./data/generic.tdm.",
                               i,
                               ".RData",
                               sep=""
                               )
    
    ## Save the raw corpus as both a generic file and a date-stamped
    ## file for recordkeeping
    save(tdm.corpus,
         house.data,
         file=file.name
         )
    save(tdm.corpus,
         house.data,
         file=timestamp.file.name
         )
  }
