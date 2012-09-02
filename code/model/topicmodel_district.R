library(tm)
library(topicmodels)
library(RWeka)
library(stringr)

load("./data/doc_term_mat/tdm.sparse.1.topicmodel.aggregate.RData")
candidates <- read.csv("./data/candidates.final.2012.csv")
districts <- read.csv("./data/districts.csv",
                      stringsAsFactors=FALSE
                      )

## Drop specialized phrases that aren't useful here
idx.drop <- which(colnames(tdm.sparse) %in%
                  c("congressman",
                    "congresswoman",
                    "rep",
                    "candidate"
                    )
                  )
tdm.sparse <- tdm.sparse[,-idx.drop]

## Drop rows with no data
which.nonzero <- which(rowSums(as.matrix(tdm.sparse)) >0)
tdm.sparse <- tdm.sparse[which.nonzero,]

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

## Drop the "dummy" language here for decency's sake
topic.labels <- str_replace_all(topic.labels, "dummy", "")
topics.lda.district <- topics(tm.lda.district)

df.district.topics <- cbind(rownames(tdm.sparse),
                            topics.lda.district,
                            as.character(topic.labels[topics.lda.district])
                            )

colnames(df.district.topics) <- c("state_dist",
                                  "topic.num",
                                  "topic.label"
                                  )

df.district.topics <- merge(df.district.topics,
                            districts,
                            by="state_dist",
                            all.x=TRUE,
                            all.y=FALSE
                            )
df.district.topics$date <- Sys.Date()

df.district.topics$state_dist <-
  as.character(df.district.topics$state_dist)
df.district.topics$topic.num <-
  as.integer(as.character(df.district.topics$topic.num))
df.district.topics$topic.label <-
  as.character(df.district.topics$topic.label)
df.district.topics$state <-
  as.character(df.district.topics$state)
df.district.topics$district <-
  as.integer(as.character(df.district.topics$district))
df.district.topics$incumbent_party <-
  as.character(df.district.topics$incumbent_party)
df.district.topics$rating <-
  as.character(df.district.topics$rating)
df.district.topics$date <-
  as.Date(as.character(df.district.topics$date))

topicmodel.ldamodel.filename <-
  paste("./data/topic_models/district_lda_model.",
        Sys.Date(),
        ".RData",
        sep=""
        )
save(tm.lda.district,
     file=topicmodel.ldamodel.filename
     )


topic.district.filename <-
  paste("./data/topic_models/district.topic.unigrams.",
        Sys.Date(),
        ".csv",
        sep=""
        )
topic.district.master.filename <-
  "./data/topic_models/district.topic.unigrams.master.csv"

write.csv(df.district.topics,
          file=topic.district.filename,
          row.names=FALSE,
          fileEncoding="UTF-8"
          )

if(file.exists(topic.district.master.filename))
  {

    master <- read.csv(topic.district.master.filename,
                       colClasses=colClasses,
                       fileEncoding="UTF-8"
                       )
    master <- rbind(master,
                    df.district.topics
                    )
    write.csv(master,
              file=topic.district.master.filename,
              row.names=FALSE,
              fileEncoding="UTF-8"
              )

  }else{

    write.csv(df.district.topics,
              file=topic.district.master.filename,
              row.names=FALSE,
              fileEncoding="UTF-8"
              )

  }

## Print diagnostics to a log file

log.file.name <- paste("./logs/topic_models/topicmodel.log.file.",
                       Sys.Date(),
                       ".log",
                       sep=""
                       )

sink(log.file.name)
print("Topic term labels:\n")
print(topic.labels)

print("Topic distribution by state:\n")
print(table(df.district.topics$state,
            df.district.topics$topic.num
            )
      )

print("Topic distribution by incumbent party:\n")
print(table(as.factor(df.district.topics$incumbent_party),
            df.district.topics$topic.num
            )
      )
sink()
