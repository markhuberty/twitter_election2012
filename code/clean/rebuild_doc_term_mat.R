## 18 November 2012
## Mark Huberty
## Alteration of the build_doc_term_mat code to rebuild
## the files from the original cron_job output. Testing to resolve
## a data loss problem in the original code.



print(options("encoding"))
print(Sys.getlocale("LC_CTYPE"))

library(tm)
library(RWeka)
library(foreach)

source("./code/util/twitter.R")

## Load the master cron file

## load("./data/cron_output/cron.file.daily.latest.RData")
## master.cron.file <- file.today.parsed.en
rm(file.today.parsed.en)
## Load the dictionaries for doc-term matrix construction
load("./data/tm_winloss_dict.RData")
load("./data/tm_voteshare_dict.RData")
tm.voteshare.dictionary <- Dictionary(tm.voteshare.dictionary)
tm.winloss.dictionary <- Dictionary(tm.winloss.dictionary)

## Load the candidate data
candidates <- read.csv("./data/candidates.final.2012.csv")
print("Inputs loaded")
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


## Start here. Need to load each daily cron job file
## from 9-17 to 11-06, build it up, and concatenate appropriately.
## Want to make sure you have tests in place to check for dropped
## data.

dateseq <- seq.Date(as.Date("2012-09-17"), as.Date("2012-11-06"), "day")

first.time <- TRUE
for(cron.date in dateseq){

  cron.filename <- paste("./data/cron_output/cron.file.",
                         cron.date,
                         ".RData",
                         sep=""
                         )
  load(cron.filename)
  master.cron.file <- file.today.parsed.en
  rm(file.today.parsed.en)

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
                             "touchdown",
                             "sports",
                             "coach",
                             "Yankees",
                             "ObliviousNFLRef"
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
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)



  voteshare.properties <- list("voteshare", 2, tm.voteshare.dictionary)
  winloss.properties <- list("winloss", 2, tm.winloss.dictionary)
  topicmodel.properties <- list("topicmodel",1, NULL)
  names(voteshare.properties) <-
    names(winloss.properties) <-
      names(topicmodel.properties) <- c("type", "ngram", "dict")

  properties.list <- list(voteshare.properties,
                          winloss.properties,
                          topicmodel.properties
                          )
  names(properties.list) <- c("voteshare",
                              "winloss",
                              "topicmodel"
                              )

  house.data.temp <- house.data

  for(l in properties.list){
    for(n in l$ngram){

      tokenizer.control <- Weka_control(min=n, max=n)
      my.tokenizer <-
        function(x) NGramTokenizer(x, tokenizer.control)
      print(l$type)
      print(n)
      ## Build up the corpus with the appropriate dictionary
      tdm.daily.corpus <- DocumentTermMatrix(corpus,
                                             control=list(tokenize=my.tokenizer,
                                               weighting=weightTf,
                                               dictionary=l$dict)
                                             )

      if(first.time)
        {

          tdm.corpus <- tdm.daily.corpus
          house.data.master <- house.data.temp
          first.time <- FALSE

        }else{
          print("Dim of new corpus")
          print(dim(tdm.daily.corpus))
          print("Dim of master corpus")
          print(dim(tdm.corpus))

          if(l$type == "topicmodel")
            {

              tdm.corpus <- c(tdm.corpus, tdm.daily.corpus)

            }else{
              tdm.corpus <- rbind(tdm.corpus,
                                  tdm.daily.corpus
                                  )
              tdm.corpus <- as.DocumentTermMatrix(tdm.corpus, weighting=weightTf)
            }
          print("Dim of master corpus after bind")
          print(dim(tdm.corpus))

          print("Dim of new house data")
          print(dim(house.data.temp))
          print("Dim of old house data")
          print(dim(house.data.master))
          house.data <- rbind(house.data.master, house.data.temp)
          print("Dim of house data after bind")
          print(dim(house.data.master))


        }



      ## Append. Note this is done the weird way b/c the
      ## c() method described in tm() dumps columns if they contain no
      ## values. Bad for the predictors later on.

      ## Generate the date-stamped file
      today <- Sys.Date()
      timestamp.file.name <- paste("./data/doc_term_mat/rebuild/generic.tdm.",
                                   n,
                                   ".",
                                   l$type,
                                   ".",
                                   today,
                                   ".RData",
                                   sep=""
                                   )

      ## Generate the generic file
      generic.file.name <- paste("./data/doc_term_mat/rebuild/generic.tdm.",
                                 n,
                                 ".",
                                 l$type,
                                 ".RData",
                                 sep=""
                                 )
      print(generic.file.name)
      print(timestamp.file.name)
      ## Save the raw corpus as both a generic file and a date-stamped
      ## file for recordkeeping
      save(tdm.daily.corpus,
           house.data.temp,
           file=generic.file.name
           )
      save(tdm.daily.corpus,
           house.data.temp,
           file=timestamp.file.name
           )
      save(tdm.corpus,
           house.data,
           file=master.corpus.filename
           )

      timestamp.master.corpus.filename <-
        paste("./data/doc_term_mat/rebuild/generic.tdm.master.",
              n,
              ".",
              l$type,
              ".",
              today,
              ".RData",
              sep=""
              )
      save(tdm.corpus,
           house.data,
           file=timestamp.master.corpus.filename
           )
    } ## end ngram
  } ## end properties


} ## END CRON FOR LOOP


## ## Save the daily output for recordkeeping
## generic.filename <- "./data/doc_term_mat/generic.corpus.daily.latest.RData"
## timestamp.filename <- paste("./data/doc_term_mat/generic.corpus.daily.",
##                             Sys.Date(),
##                             ".RData",
##                             sep=""
##                             )

## save(corpus, house.data,
##      file=generic.filename
##     )
## save(corpus, house.data,
##      file=timestamp.filename
##     )
