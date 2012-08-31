## Begun 22 Jan 2011
## Purpose:
## Concatenate the by-candidate aggregate tweet corpus
## to a district-level corpus for all two-candidate districts
## Then predict party outcome/vote

if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/twitter_election2010")
  }else{
    setwd("~/Documents/twitter_election2010")
  }


library(SuperLearner)
#library(tm)
#library(RWeka)
library(ggplot2)
library(foreach)
#library(doMC)
#library(kernlab)

library(glmnet)
library(e1071)
library(randomForest)

source("./code/twitter.R")
source("./code/build_sparse_functions.R")
## output from build.district.corpus.R

## Note the term weigting here. With the undifferentiated
## candidate list, used simple binary weighting
## Here, need to use the actualy freqs to get
## relative measures inside the race corpus

filename <- "tdm.sparse.20.01.aggregate.scale.linear"
load(paste("./data/", filename, ".RData",  sep=""))
corpus.district.tdm.mat <- select.tfidf(tdm.sparse, threshold=0.0005)
rm(tdm.sparse)
gc()

## Take the output vector
cand.data.dist <- house.data[,
                             c("state", "district", "d.victory")
                             ]
cand.data.dist <- unique(cand.data.dist)
rm(house.data)
gc()

## Make sure that the input and output data are in the same order (st-dist)
sort.vec <- paste(cand.data.dist$state, cand.data.dist$district,
                  sep=".")
cand.data.dist <- cand.data.dist[order(sort.vec),]
rm(sort.vec)

corpus.district.tdm.mat <-
  corpus.district.tdm.mat[order(rownames(corpus.district.tdm.mat)),]

orig.names <- colnames(corpus.district.tdm.mat)
new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
colnames(corpus.district.tdm.mat) <- new.names
rm(new.names)

## Scaled the data so columns are all mean=0, sd=1
corpus.district.tdm.mat.scale <- scale(corpus.district.tdm.mat)



tuneGrid.rf <- expand.grid(mtry = c(500, 1000, 2200), nodesize = c(1, 5,
                                                            10))

## Generate the algorithm library
for(mm in seq(nrow(tuneGrid.rf))) { 
  eval(parse(file = "", text = paste("SL.randomForest.",
                          mm, 
                          "<- function(..., mtry = ",
                          tuneGrid.rf[mm, 1],
                          ", nodesize = ", 
                          tuneGrid.rf[mm, 2],
                          ") { SL.randomForest(..., mtry = mtry, 
 nodesize = nodesize) }",
                          sep = "")
             )
       ) 
}

SL.library.discrete <- list(c('SL.svm', 'screen.glmnet'),
                            c('SL.glmnet', 'screen.glmnet')
                            )
SL.library.discrete.vec <- c('SL.randomForest',
                             paste("SL.randomForest.",
                                   seq(nrow(tuneGrid.rf)),
                                   sep = ""),
                             'SL.svm.nusvc02',
                             'SL.svm.nusvc04',
                             'SL.svm.nusvc06',
                             'SL.svm.c',
                             'SL.svm.c.10'
                             ) 
SL.library.discrete <- c(SL.library.discrete,
                         SL.library.discrete.vec
                         )
rm(SL.library.discrete.vec)

is.var.constant <- function(x){
  x <- var(x, na.rm=TRUE)
  out <- x == 0
  return(out)
}

## Run the loop
N <- 500
counter <- 0
set.seed(12323)
list.out <- list()
while(counter <= N)
  {

    n.train <- ceiling(0.8 * dim(corpus.district.tdm.mat)[1])
    sample.vec <- sample(1:dim(corpus.district.tdm.mat)[1], n.train)
    mat.train <- corpus.district.tdm.mat.scale[sample.vec,]
    mat.test <- corpus.district.tdm.mat.scale[-sample.vec,]

    out.train.discrete <- cand.data.dist$d.victory[sample.vec]
    out.test.discrete <- cand.data.dist$d.victory[-sample.vec]

    data.train <- as.data.frame(mat.train)
    data.test <- as.data.frame(mat.test)

    rm(mat.train, mat.test)
    gc()
    gc()

    ## Test for validity
    test.train.out <- is.var.constant(out.train.discrete)
    test.train.var <- apply(data.train, 2, is.var.constant)
    
    ## If the tests fail, 
    if(test.train.out | 
       sum(test.train.var) > 0
       )
      {

        print("Skipping bad sample")
        next

      }else{

        counter <- counter + 1

      }
    
    
    rm(test.train.out, test.train.var)
    
    ## Train the superlearner
    train.superlearner.discrete <- SuperLearner(Y=out.train.discrete,
                                                X=data.train,
                                                newX=data.train,
                                                SL.library=SL.library.discrete,
                                                V=5,
                                                family=binomial(),
                                                verbose=TRUE,
                                                shuffle=FALSE
                                                )

    ## Generate the predictions
    SL.predict.discrete <- predict(train.superlearner.discrete)
    SL.predict.discrete.oos <- predict(train.superlearner.discrete,
                                       newdata=data.test
                                       )


    SL.predict.discrete.binary <- ifelse(SL.predict.discrete > 0.5,
                                         1,
                                         0
                                         )
    SL.predict.discrete.oos.bin <- ifelse(SL.predict.discrete.oos$fit > 0.5,
                                          1,
                                          0
                                          )


    tab.sl.predict.discrete <- table(as.factor(SL.predict.discrete.binary),
                                     as.factor(out.train.discrete)
                                     )
    tab.sl.predict.discrete.oos <- table(as.factor(SL.predict.discrete.oos.bin),
                                         as.factor(out.test.discrete)
                                         )

    ## Generate the accuracy figures and print
    pct.correct.sl.disc <- sum(diag(tab.sl.predict.discrete)) /
      sum(tab.sl.predict.discrete)

    pct.correct.oos <- sum(diag(tab.sl.predict.discrete.oos)) /
      sum(tab.sl.predict.discrete.oos)

    list.out[[counter]] <- c(pct.correct.sl.disc,
                             pct.correct.oos
                             )

    print(paste("Finished with run", counter))



  }


accuracy.rates <- sapply(list.out, function(x){x})
print(dim(accuracy.rates))

mean.accuracy.rate <- rowMeans(accuracy.rates)
sd.accuracy.rate <- c(sd(accuracy.rates[1,]),
                      sd(accuracy.rates[2,])
                      )

print("Mean accuracy rate")
print(mean.accuracy.rate)
print("SD accuracy rates")
print(sd.accuracy.rate)
      
pdf("./plots/bs_accuracy_density_pdf")
plot(density(accuracy.rates[2,]),
     xlab="OOS accuracy rate",
     ylab="",
     main="Distribution of OOS accuracy, N=500"
     )
dev.off()

## Save the entire image and exit
file.out <- paste("./data/",
                  "district.superlearner.discrete.",
                  filename,
                  ".tfidf.oosbootstrap",
                  ".RData",
                  sep=""
                  )
save.image(file.out)


