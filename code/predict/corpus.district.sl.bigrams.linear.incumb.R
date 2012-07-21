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

load("./data/twitter.data.house.results.RData")
df <- unique(twitter.data.house.results[,c("state",
                                           "district",
                                           "party",
                                           "incumbent.x")
                                        ]
             )
names(df) <- c("state", "district", "party", "incumbent.x")
df <- df[df$party %in% c("D", "R"),]
df$chal.idx <- NA

chal.idx <- find.chal.idx(df$state, df$district, df$party)

df$chal.idx[chal.idx[,1]] <- chal.idx[,2]

df <- df[!is.na(df$chal.idx),]

chal.idx <- find.chal.idx(df$state, df$district, df$party)

df$chal.idx[chal.idx[,1]] <- chal.idx[,2]


df$incumbent.party <- NA
## Then establish the incumbent party

for(i in 1:nrow(df))
  {



    if((df$incumbent.x[i] == 1 & df$party[i] == "D") |
       (df$incumbent.x[i] == 0 & df$party[i] == "R"))
       df$incumbent.party[i] <- "D"
    
    if((df$incumbent.x[i] == 1 & df$party[i] == "R") |
       (df$incumbent.x[i] == 0 & df$party[i] == "D"))
       df$incumbent.party[i] <- "R"

    if(df$incumbent.x[i] == 0 & df$incumbent.x[df$chal.idx[i]] == 0)
      df$incumbent.party[i] <- "N"
       
  }
rm(twitter.data.house.results)
gc()

## Take the output vector
cand.data.dist <- house.data[,
                             c("state", "district",
                               "d.victory")
                             ]
cand.data.dist <- unique(cand.data.dist)

cand.data.dist <- merge(cand.data.dist,
                        df,
                        by.x=c("state", "district"),
                        by.y=c("state", "district"),
                        all.x=TRUE,
                        all.y=FALSE
                        )

rm(house.data)
gc()

## Transform the incumbent data into 1/0 cols

## Make sure that the input and output data are in the same order (st-dist)
sort.vec <- paste(cand.data.dist$state, cand.data.dist$district,
                  sep=".")
cand.data.dist <- cand.data.dist[order(sort.vec),]
rm(sort.vec)

incumb.dv <- sapply(levels(factor(cand.data.dist$incumbent)), function(x){

  ifelse(cand.data.dist$incumbent == x, 1, 0)

})
names(incumb.dv) <- c("incumb.d", "incumb.n", "incumb.r") 

corpus.district.tdm.mat <-
  corpus.district.tdm.mat[order(rownames(corpus.district.tdm.mat)),]

corpus.district.tdm.mat <- cbind(corpus.district.tdm.mat,
                                 incumb.dv
                                 )


orig.names <- colnames(corpus.district.tdm.mat)
new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
colnames(corpus.district.tdm.mat) <- new.names
rm(new.names)

## Scaled the data so columns are all mean=0, sd=1
corpus.district.tdm.mat.scale <- scale(corpus.district.tdm.mat)

## Create the train and test datasets
## set.seed(82345)
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

tuneGrid.rf <- expand.grid(mtry = c(500, 1000, 2200), nodesize = c(1, 5,
                                                     10))

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

## tuneGrid.svm.nu <- expand.grid(kernel=c("linear", "radial"), nu =
##                                seq(0.2, 0.8, 0.2)
##                                )

## for(mm in seq(nrow(tuneGrid.svm.nu))){

##   eval(parse(file="", text=paste("SL.svm.",
##                         mm,
##                         "<- function(..., kernel=",
##                         tuneGrid.svm.nu[mm, 1],
##                         ", nu=",
##                         tuneGrid.svm.nu[mm, 2],
##                         ") {SL.svm(..., kernel=kernel, nu=nu, type.class='nu-classification')}",
##                         sep="")
##              )
##        )
## }

## tuneGrid.svm.c <- expand.grid(kernel=c("linear", "radial"),
##                               cost=seq(0.5, 5, 0.5),
                              

## Establish the superlearner libraries
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

## Generate the in-sample predictions
SL.predict.discrete <- predict(train.superlearner.discrete)

SL.predict.discrete.binary <- ifelse(SL.predict.discrete > 0.5,
                                     1,
                                     0
                                     )
tab.sl.predict.discrete <- table(as.factor(SL.predict.discrete.binary),
                                 as.factor(out.train.discrete)
                                 )

## Generate the accuracy figures and print
pct.correct.sl.disc <- sum(diag(tab.sl.predict.discrete)) /
  sum(tab.sl.predict.discrete)
print(tab.sl.predict.discrete)
print(pct.correct.sl.disc)

## Get the out-of-sample prediction accuracy estimates
SL.predict.discrete.oos <- predict(train.superlearner.discrete,
                                   newdata=data.test
                                   )

SL.predict.discrete.oos.bin <- ifelse(SL.predict.discrete.oos$fit > 0.5,
                                      1,
                                      0
                                      )

tab.sl.predict.discrete.oos <- table(as.factor(SL.predict.discrete.oos.bin),
                                     as.factor(out.test.discrete)
                                     )
print(tab.sl.predict.discrete.oos)
pct.correct.oos <- sum(diag(tab.sl.predict.discrete.oos)) /
                                     sum(tab.sl.predict.discrete.oos)
print(pct.correct.oos)

## Save the entire image and exit
file.out <- paste("./data/",
                  "district.superlearner.discrete.",
                  filename,
                  ".tfidf",
                  ".RData",
                  sep=""
                  )
save.image(file.out)


