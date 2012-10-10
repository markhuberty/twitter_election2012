##
## Code to build a new version of the DTM
## That works by summing the row-counts rather than
## concatenating tweets. Cat'ing tweets gives the
## problem of bigrams at the junction

## This is Mark's build_doc_term_mat.R code with a few changes to create 
## summary data on opinionfinder wordcounts per candidate, over tweets. 
## Final product is a candidate-level dataframe. -hs

print(options("encoding"))
print(Sys.getlocale("LC_CTYPE"))

library(tm)
library(RWeka)
library(foreach)

# For fiddling purposes, to be deleted:
# setwd("/Users/hills/t1/twitter_election2012")
# source("./code/util/build_sparse_functions.R")
source("./code/util/twitter.R")
# load("../../master.cron.file.Rdata")
load("./data/cron_output/master.cron.file.Rdata")

# For fiddling purposes, to be deleted:
# master.cron.file <- master.cron.file[5000:15000,]

# Strong-subject vs weak-subject opinionfinder weights:
weights <- c(1, .5)


## Load the dictionaries for doc-term matrix construction
wordlist <- read.csv("../opinionfinder_wordlist.csv")

wordlist <- wordlist[!(wordlist$priorpolarity=="neutral"), ]
wordlist <- wordlist[!wordlist$priorpolarity=="both", ]
wordlist <- wordlist[!duplicated(wordlist$word1), ]

wordlist$priorpolarity <- as.character(wordlist$priorpolarity)
wordlist$priorpolarity[wordlist$priorpolarity=="positive"] <- 1
wordlist$priorpolarity[wordlist$priorpolarity=="negative"] <- -1
wordlist$priorpolarity <- as.numeric(wordlist$priorpolarity)

wordlist$type <- as.character(wordlist$type)
wordlist$type[wordlist$type=="strongsubj"] <- weights[1]
wordlist$type[wordlist$type=="weaksubj"] <- weights[2]
wordlist$type <- as.numeric(wordlist$type)

wordlist.dictionary <- Dictionary(as.character(wordlist$word1))

## Load the candidate data
candidates <- read.csv("./data/candidates.final.2012.csv")
print("Inputs loaded")


## Format the dates correctly
time.format <-  "%a, %d %b %Y %H:%M:%S %z"
formatted.date <-
  strptime(as.character(master.cron.file$created_at), time.format)
time.format <- "%Y-%m-%d"
formatted.date <-
  strptime(as.character(formatted.date), time.format)
master.cron.file$created_at <- formatted.date

house.data <- master.cron.file
rm( master.cron.file)

## Make sure that all tweets come from before the election
election.date <- as.Date("2012-11-6", format="%Y-%m-%d")
formatted.election.date <- strptime(election.date, time.format)
# MAKE as numeric --> number of seconds since 1960 or something similar. -h
before.date <-
  as.numeric(house.data$created_at) < as.numeric(formatted.election.date)
house.data <- house.data[before.date, ]
rm(before.date)

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

print("Dates and ages formatted")

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

print("Merge complete")

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

print("Challengers subsetted")

## Now clear out the junk in the tweets themselves
## To prevent an error when certain tweets have non-unicode-8 characters, encode to latin-1.
#Encoding(house.data$text) <- "latin1"
house.data$text <- iconv(house.data$text,
                         "UTF-8",
                         "ASCII",
                         sub=" "
)
corpus <- tolower(house.data$text)
print("Encoding fixed")

## Hack out noisy results
noise.indicator.terms <- c("mlb",
                           "kicker",
                           "orleans",
                           "yankee",
                           "nfl",
                           "yankees",
                           "baseball",
                           "football",
                           "orioles",
                           "touchdown"
)
noise.indicator.terms <- c(paste(" ", noise.indicator.terms, sep=""),
                           paste(noise.indicator.terms, " ", sep="")
)

which.noise <- sapply(noise.indicator.terms,
                      function(x) str_detect(corpus,
                                             fixed(x)
                      )
)
which.noise <- rowSums(which.noise) > 0

print("Tweet count before noise exclusion")
print(nrow(house.data))
corpus <- corpus[!which.noise]
house.data <- house.data[!which.noise, ] # Added !
print("Tweet count after noise exclusion")
print(nrow(house.data))


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
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)

tokenizer.control <- Weka_control(min=1, max=1)
my.tokenizer <-
  function(x) NGramTokenizer(x, tokenizer.control)

## Determine number of tweets per candidate.
## Will later determine number of opinion-finder tweets per candidate.
tweets.per.cand <- as.matrix(table(house.data$unique_cand_id))

## Build up the corpus with the appropriate dictionary
###### Use you wordlist dictionary.
dtm <- DocumentTermMatrix(corpus,
                          control=list(tokenize=my.tokenizer,
                                         weighting=weightTf,
                                        dictionary=wordlist.dictionary))

# Rows=tweets, cols=words
non.zero.rows <- sort(unique(dtm$i))
non.zero.cols <- sort(unique(dtm$j))
corpus <- corpus[non.zero.rows]
house.data <- house.data[non.zero.rows, ]
dtm <- dtm[non.zero.rows, non.zero.cols]
words <- wordlist[non.zero.cols, ]
weights <- (words$type)*(words$priorpolarity)

## Determine number of tweets with opinion-finder words per candidate.
op_tweets_per_cand <- as.matrix(table(house.data$unique_cand_id))

# Aggregate sparse matrix by candidate.
# The c-code wasn't working, so I just did this for now:
sparse.cands <- house.data$unique_cand_id[dtm$i]
idx <- cbind(dtm$i, dtm$j, dtm$v)
unique.cands <- (unique(sparse.cands))

for(i in 1:length(unique(sparse.cands))){
  cand.idx <- which(sparse.cands == unique.cands[i])
  idx[cand.idx, 1] <- i
  if(length(cand.idx)>1){
    idx[cand.idx, ] <- idx[cand.idx, ][order(idx[cand.idx, 2]), ]
  }
}

for(i in 1:length(unique(sparse.cands))){
  cand.idx <- which(sparse.cands == unique.cands[i])
  norep <- !duplicated(idx[cand.idx,2])
  for(j in which(norep)[
    (idx[cand.idx,2][norep] %in%
      idx[cand.idx,2][(duplicated(idx[cand.idx,2]))]) 
    ]){
      idx[cand.idx[j],3] <- sum(idx[which(idx[cand.idx,2]==idx[cand.idx[j],2]),3])
    }        
  }

remove <- which(duplicated(idx[,1:2]))
idx <- idx[-remove, ]

# Now push new values into the dtm matrix.
dtm <- dtm[1:length(unique.cands), ]
dtm$i <- idx[,1]
dtm$j <- idx[,2]
dtm$v <- idx[,3]

rownames(dtm) <- unique.cands
# Now create a matrix. At max, this matrix will be ≈ 400x6000, 
# So not terribly large.
opinion.matrix <- as.matrix(dtm)
opinion.matrix <- t(t(opinion.matrix)*weights)
by.candidate <- rowSums(opinion.matrix)

# Need a csv file like the following:
# state:district:party:sentiment_score:sentiment_date

state <- substr(rownames(opinion.matrix), 1, 2)
district <- substr(rownames(opinion.matrix), 3, 4)
party <- substr(rownames(opinion.matrix), 6, 6)
sentiment_score <- by.candidate
sentiment_date <- rep(Sys.Date(), nrow(opinion.matrix))

# Merge the two types of number of tweets per candidate
tweets_per_cand <- as.matrix(tweets.per.cand[
  rownames(tweets.per.cand) %in% rownames(op.tweets.per.cand), 1
  ])
standardized_score <- (sentiment_score/tweets.per.cand)

by.cand <- data.frame(state, district, party, sentiment_score, 
                      tweets.per.cand, op.tweets.per.cand, sentiment_date)

barplot(by.candidate, col=c("blue", "red")[as.numeric(factor(party))],
        pch=19, xlab="Candidate", ylab="Sentiment Score", names.arg="")
  
plot(1:length(by.candidate), by.candidate,
     col=c("blue", "red")[as.numeric(factor(party))],
     pch=19, xlab="Candidate", ylab="Sentiment Score", cex=abs(by.candidate)^.2)
  
  
plot(standardized_score, col=c("#1111FF70", "#FF111170")[as.numeric(factor(party))],
     pch=19, cex=tweets.per.cand^.2)
abline(h=mean(standardized_score[party=="D"]), col="#1111FF99", lwd=4)
abline(h=mean(standardized_score[party=="R"]), col="#FF111199", lwd=4)
  
  