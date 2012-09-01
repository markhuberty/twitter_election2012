library(tm)
library(topicmodels)
library(RWeka)

load("./data/doc_term_mat/generic.corpus.RData")
candidates <- read.csv("./data/candidates.final.2012.csv")
source("./code/util/build_sparse_functions.R")

## Generate the input data
my.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2,
                                                           max = 2)
                                           )
corpus <-
corpus.district.tdm <- TermDocumentMatrix(corpus,
                                          control=list(tokenize=my.tokenizer,
                                            weighting=weightTf)
                                          )
agg.fac.dist <- house.data$state_dist

sparseness <- 0.99
corpus.district.tdm.sparse <-
  if(sparseness < 1){
    removeSparseTerms(corpus.district.tdm,
                      sparseness)
  }else{
    corpus.district.tdm
  }


corpus.district.tdm.mat <- t(as.matrix(corpus.district.tdm.sparse))
print(dim(corpus.district.tdm.mat))

## Try to drop some stuff
idx.drop <- which(colnames(corpus.district.tdm.mat) %in%
                  c("rep dcanddummy",
                    "rep rcanddummy",
                    "support dcanddummy",
                    "support rcanddummy",
                    "congressman dcanddummy",
                    "congressman rcanddummy",
                    "rcanddummy congress",
                    "dcanddummy congress",
                    "candidate dcanddummy",
                    "candidate rcanddummy",
                    "congresswoman dcanddummy",
                    "congresswoman rcanddummy",
                    )
                  )
corpus.district.tdm.mat <- corpus.district.tdm.mat[,-idx.drop]

## Begin topic modeling
## Minimize perplexity to choose topic counts


seed <- 3423
k.values <- seq(4, 30, 2)
n.train <- floor(0.9 * nrow(corpus.district.tdm.mat))
sample.vec <- sample(1:nrow(corpus.district.tdm.mat),
                     n.train,
                     replace=FALSE
                     )
train.data <- corpus.district.tdm.mat[sample.vec, ]
test.data <- corpus.district.tdm.mat[-sample.vec, ]

perplexity.trial <- lapply(k.values, function(x){
  out <- LDA(train.data, k=x, control=list(seed=seed))
  predict.out <- LDA(train.data, model=out)
  loglik <- logLik(predict.out)
  return(loglik)

})

which.k <- which.max(unlist(perplexity.trial))
k.best <- k.values[which.k]


tm.lda.district <- LDA(corpus.district.tdm.mat,
                       k=k.best,
                       control=list(seed=seed)
                       )

terms.lda.district <- terms(tm.lda.district, 5)
topics.lda.district <- topics(tm.lda.district)

df.district.topics <- cbind(house.data$state,
                            house.data$district,
                            topics.lda.district
                            )
colnames(df.district.topics) <- c("state",
                                  "district",
                                  "topic.num"
                                  )
df.district.topics$state.district <-
  generate.state.district.code(df.district.topics$state,
                               df.district.topics$district
                               )


topic.term.map <-
  sapply(1:ncol(terms.lda.district),
         function(x){

           terms <- paste(terms.lda.district[, this.topic],
                          collapse="."
                          )
           out <- c(x, ,
                    terms
                    )

         }
         )
topic.term.map <- t(topic.term.map)
colnames(topic.term.map) <- c("topic.num", "topic.label")

topic.map.filename <- paste("./data/topic.term.map.",
                            Sys.Date(),
                            ".csv"
                            )
topic.map.master.filename <- "./data/topic.term.map.latest.csv"
topic.district.filename <- paste("./data/topic.district.map."
                                 Sys.Date(),
                                 ".csv"
                                 )
topic.district.master.filename <-
  "./data/topic.district.latest.csv"

## TODO need to ket the right kind of date stamping here for master
## files
write.csv(topic.term.map,
          file=topic.map.filename,
          row.names=FALSE
          )
write.csv(df.district.topics,
          file=topic.district.filename,
          row.names=FALSE
          )

write.csv(topic.term.map,
          file=topic.map.filename,
          row.names=FALSE
          )
write.csv(df.district.topics,
          file=topic.district.filename,
          row.names=FALSE
          )




## Other? Can we automate quality checks of some kind?
## Print some diagnostics? table by party, state,
## compare to topic coherence from prior day?
##


## Print diagnostics to a log file
## Note /  need to make sure that we're subsetting the
## house data correctly here.
log.file.name <- paste("topicmodel.log.file.",
                       Sys.Date(),
                       ".log",
                       sep=""
                       )

sink(log.file.name)
print("Topic term labels:\n")
print(topic.term.map)

print("Topic distribution by state:\n")
table(df.district.topics$state,
      df.district.topics$topic.num
      )

print("Topic distribution by incumbent party:\n")
table(house.data$incumbent.party,
      df.district.topics$topic.num
      )
sink()
