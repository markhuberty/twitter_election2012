## Code to model the topics by district
## Begun 22 September 2011
## Mark Huberty

if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/twitter_election2010")
  }else{
    setwd("~/Documents/twitter_election2010")
  }


library(tm)
library(topicmodels)
library(RWeka)
library(foreach)
library(doMC)

load("./data/corpus.district.RData")


my.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2,
                                                           max = 2)
                                           )

corpus.district.tdm <- TermDocumentMatrix(corpus.district,
                                          control=list(tokenize=my.tokenizer,
                                            weighting=weightTf)
                                          )

## Then strip out the sparseness. For this dataset,
## 0.95 yields about 1411 terms
sparseness <- 0.99

corpus.district.tdm.sparse <- if(sparseness < 1){
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
                    "cliff lee"
                   )
                  )
corpus.district.tdm.mat <- corpus.district.tdm.mat[,-idx.drop]
corpus.district.tdm.mat <-
  as.DocumentTermMatrix(corpus.district.tdm.mat,
                        weighting=weightTf
                        )
## Begin topic models

seed <- 3423

## Check range of K values, maximize held-out loglik and
## minimize perplexity
k.values <- seq(5, 50, 5)
sample.vec <- sample(1:nrow(corpus.district.tdm.mat),
                     floor(0.9 * nrow(corpus.district.tdm.mat)),
                     replace=FALSE
                     )
train.data <- corpus.district.tdm.mat[sample.vec,]
test.data <- corpus.district.tdm.mat[-sample.vec,]
test.data <- test.data[rowSums(as.matrix(test.data)) > 0,]

registerDoMC(3)
loglik.lda <- foreach(x = k.values, .combine=rbind) %dopar% {

  print(x)
  tm.lda.district <- LDA(corpus.district.tdm.mat,
                         k=x,
                         control=list(seed=seed)
                         )
  predict.lda <- LDA(test.data, model=tm.lda.district)
  loglik <- logLik(predict.lda)
  perplex <- perplexity(predict.lda)

  return(c(loglik, perplex))

}

print(loglike.lda)
save(k.values, loglik.lda, seed,
     file="district_lda_loglik.RData"
     )
quit()


