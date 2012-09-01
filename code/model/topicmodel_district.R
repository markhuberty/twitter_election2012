library(tm)
library(topicmodels)
library(RWeka)

load("./data/doc_term_mat/tdm.sparse.2.topicmodel.aggregate.RData")
candidates <- read.csv("./data/candidates.final.2012.csv")

## Drop specialized phrases that aren't useful here
idx.drop <- which(colnames(tdm.sparse) %in%
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
                    "congresswoman rcanddummy"
                    )
                  )
tdm.sparse <- tdm.sparse[,-idx.drop]

## Set the k value to maximize the log-lik of the model
## in held-out data
seed <- 3423
k.values <- seq(4, 30, 2)
n.train <- floor(0.9 * nrow(tdm.sparse))
sample.vec <- sample(1:nrow(tdm.sparse),
                     n.train,
                     replace=FALSE
                     )
train.data <- tdm.sparse[sample.vec, ]
test.data <- tdm.sparse[-sample.vec, ]

loglik.trial <- lapply(k.values, function(x){
  out <- LDA(train.data, k=x, control=list(seed=seed))
  predict.out <- LDA(train.data, model=out,
                     estimate.beta=FALSE
                     )
  loglik <- logLik(predict.out)
  return(loglik)

})

which.k <- which.max(unlist(loglik.trial))

tm.lda.district <- LDA(tdm.sparse,
                       k=k.values[which.k],
                       control=list(seed=seed)
                       )

terms.lda.district <- terms(tm.lda.district, 5)
topic.labels <- apply(terms.lda.district,
                      2,
                      function(x) paste(x, collapse="|")
                      )
topics.lda.district <- topics(tm.lda.district)

df.district.topics <- cbind(rownames(tdm.sparse),
                            topics.lda.district,
                            topic.labels[topics.lda.district]
                            )

colnames(df.district.topics) <- c("state_dist",
                                  "topic.num",
                                  "topic.label"
                                  )

df.district.topics <- merge(df.district.topics,
                            candidates[candidates$incumbent=="True",
                                       c("state_dist",
                                         "state_id",
                                         "district",
                                         "incumbent.party"
                                         )
                                       ],
                            by="state_dist",
                            all.x=TRUE,
                            all.y=FALSE
                            )
df.district.topics$date <- Sys.Date()

colClasses <- c("character",
                "integer",
                "character",
                "character",
                "integer",
                "character",
                "Date"
                )

for(col in 1:ncol(df.district.topics))
  {

    class(df.district.topics[,col]) <- colClasses[col]

  }

topic.district.filename <- paste("./data/topic.district.map.",
                                 Sys.Date(),
                                 ".csv",
                                 sep=""
                                 )
topic.district.master.filename <-
  "./data/topic.district.master.csv"

write.csv(df.district.topics,
          file=topic.district.filename,
          row.names=FALSE
          )

if(file.exists(topic.district.master.filename))
  {

    master <- read.csv(topic.district.master.filename,
                       colClasses=c("character",
                         "integer",
                         "character",
                         "character",
                         "integer",
                         "character",
                         "Date"
                         )
                       )
    master <- rbind(master,
                    df.district.topics
                    )
    write.csv(master,
              file=topic.district.master.filename,
              row.names=FALSE
              )

  }else{

    write.csv(master,
              file=topic.district.master.filename,
              row.names=FALSE
              )

  }

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
print(topic.labels)

print("Topic distribution by state:\n")
table(df.district.topics$state,
      df.district.topics$topic.num
      )

print("Topic distribution by incumbent party:\n")
table(house.data$incumbent.party,
      df.district.topics$topic.num
      )
sink()
