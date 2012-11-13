## 13 November 2012
## Mark Huberty
## Script to generate cumulative master files to study why some races
## changed so much after mid-October

library(Matrix)
library(foreach)
library(tm)
library(RWeka)


source("./code/util/build_sparse_functions.R")
source("./code/util/twitter.R")

## Load up the master file for the voteshare data
load("tdm.sparse.2.voteshare.aggregate.scale.uniform.RData")

## Clean and aggregate the data; want to generate for each day
## starting 10-15 and ending on 11-6
time.format <- "%Y-%m-%d"

min.date <- as.Date(strptime("2012-10-15", time.format))
max.date <- as.Date(strptime("2012-11-06", time.format))

date.seq <- seq.Date(from=min.date, to=max.date, by="day")

tdm.sparse.byday <- lapply(1:length(date.seq), function(x){

  d <- date.seq[i]

  ## Incremental buildup
  if(i == 1)
    house.data.temp <- house.data[as.Date(house.data$created_at) <= d,]
  else
    house.data.temp <- house.data[as.Date(house.data$created_at)==d,]

  corpus <- tolower(house.data$text)
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

  tokenizer.control <- Weka_control(min=2, max=2)

  if(i == 1)
    {

      tdm.corpus <- DocumentTermMatrix(corpus,
                                       control=list(tokenizer=my.tokenizer,
                                         weighting=weightTf,
                                         dictionary=tm.voteshare.dictionary)
                                       )
      house.data <- house.data.temp

    }else{

      tdm.temp <- DocumentTermMatrix(corpus,
                                     control=list(tokenizer=my.tokenizer,
                                       weighting=weightTf,
                                       dictionary=tm.voteshare.dictionary)
                                     )
      tdm.corpus <- rbind(tdm.corpus,
                          tdm.temp
                          )
      tdm.corpus <- as.DocumentTermMatrix(tdm.corpus, weighting=weightTf)
      house.data <- rbind(house.data, house.data.temp)

    }

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



  out <- list(tdm.sparse, col.names, row.names)
  return(out)

})

save(tdm.sparse.byday, file="./data/doc_term_mat/tdm.sparse.byday.RData")
