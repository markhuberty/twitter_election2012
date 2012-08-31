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

## Load the master cron file
load("./data/cron_output/master.cron.file.RData")
## Load the dictionary for doc-term matrix construction
load("./data/tm_dictionary.RData")
## Load the candidate data
candidates <- read.csv("./data/candidates.final.2012.csv")

## This looks like:
## first.name
## last.name
## text
## created_at
## to_user
## from_user
## to_user_id
## from_user_id
## state
## district
## iso_language_code
## source
## metadata.result_type
## profile_image.url

## Format the dates correctly
 time.format <-  "%a, %d %b %Y %H:%M:%S %z"
 formatted.date <-
  strptime(as.character(master.cron.file$created_at), time.format)
 time.format <- "%Y-%m-%d"
 formatted.date <-
  strptime(as.character(formatted.date), time.format)
 master.cron.file$created_at <- formatted.date


## Subset the candidates to D/R races
# is.dem.or.rep <- master.cron.file$party %in% c("D", "R")
house.data <- master.cron.file # [is.dem.or.rep,]
# rm(is.dem.or.rep, master.cron.file)

## Make sure that all tweets come from before the election
election.date <- as.Date("2012-11-6", format="%Y-%m-%d")
# Need to make the two dates comparable: -h
formatted.election.date <- strptime(election.date, time.format)
# MAKE as numeric --> number of seconds since 1960 or something similar. -h
before.date <-
  as.numeric(house.data$created_at) < as.numeric(formatted.election.date)
house.data <- house.data[before.date, ]
# rm(election.date) -hs

## Chart the tweet age in days before election date
tweet.age <- round(as.numeric(difftime(election.date,
                                       house.data$created_at,
                                       units="weeks"
                                       )
                              )
                   , 0)
tweet.age <- max(tweet.age) - tweet.age

house.data$tweet.age <- tweet.age
rm(tweet.age)

house.data$unique_cand_id <-
  as.character(house.data$unique_cand_id)
candidates$unique_cand_id <-
  as.character(candidates$unique_cand_id)
# Merge in the party data
house.data <- merge(house.data,
                    candidates[,c("unique_cand_id",
                                  "state_dist",
                                  "state",
                                  "district",
                                  "party",
                                  "first_name",
                                  "last_name"
                                  )
                               ],
                    by="unique_cand_id",
                    all.x=TRUE,
                    all.y=FALSE
                    )

## Then make sure we are only looking at races with
## data for both candidates.
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

## Shouldn't need this stuff anymore
## house.data <- house.data[,c("state",
##                             "district",
##                             "party",
##                             "first.name.x",
##                             "last.name.x",
##                             "created.at.date",
##                             "from_user_id",
##                             "to_user_id",
##                             "text",
##                             "pctVote",
##                             "d.victory",
##                             "tweet.age",
##                             "chal.idx"
##                             )
##                          ]

## names(house.data) <- c("state",
##                        "district",
##                        "party",
##                        "first.name",
##                        "last.name",
##                        "created.at.date",
##                        "from.user.id",
##                        "to.user.id",
##                        "text",
##                        "pctVote",
##                        "d.victory",
##                        "tweet.age",
##                        "chal.idx"
##                        )


## Now clear out the junk in the tweets themselves
## To prevent an error when certain tweets have non-unicode-8 characters, encode to latin-1.
Encoding(house.data$text) <- "latin1"
corpus <- tolower(house.data$text)

## Hack out noisy results
noise.indicator.terms <- c("kicker",
                           "orleans"
                           )
which.noise <- str_detect(house.data$text,
                          paste(noise.indicator.terms,
                                collapse="|"
                                )
                          )

corpus <- corpus[!which.noise]
house.data <- house.data[!which.noise, ] # Added !

## Process the corpus in stages to format names, offices
## Remove http://* and usernames
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
                             house.data$first_name,
                             house.data$last_name,
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

generic.filename <- "./data/doc_term_mat/generic.corpus.RData"
timestamp.filename <- paste("./data/doc_term_mat/generic.corpus.",
                            Sys.Date(),
                            ".RData",
                            sep=""
                            )

save(corpus,
     file=generic.filename
    )
save(corpus,
     file=timestamp.filename
    )

# Generate the bigram dictionary
tm.dictionary <- Dictionary(corpus.colnames)

## Generate the by-tweet doc-term matrices
## 1 for topic modeling
## 2 for prediction
ngrams <- c(1,2)
dictionaries <- list(NULL,
                     tm.dictionary
                     )

for(i in ngrams){
  my.tokenizer <-
    function(x) NGramTokenizer(x, Weka_control(min = i, max = i))

  ## Build up the corpus with the appropriate dictionary
  tdm.corpus <- DocumentTermMatrix(corpus,
                                   control=list(tokenize=my.tokenizer,
                                     weighting=weightTf,
                                     dictionary=dictionaries[[i]])
                                   )

  ## Generate the date-stamped file
  today <- Sys.Date()
  timestamp.file.name <- paste("./data/doc_term_mat/generic.tdm.",
                               i,
                               ".",
                               today,
                               sep=""
                               )

  ## Generate the generic file
  generic.file.name <- paste("./data/doc_term_mat/generic.tdm.",
                             i,
                             ".RData",
                             sep=""
                             )

  ## Save the raw corpus as both a generic file and a date-stamped
  ## file for recordkeeping
  save(tdm.corpus,
       house.data,
       file=generic.file.name
       )
  save(tdm.corpus,
       house.data,
       file=timestamp.file.name
       )
}
