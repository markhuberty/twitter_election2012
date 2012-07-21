## Begun 22 Jan 2011
## Purpose:
## Concatenate the by-candidate aggregate tweet corpus
## to a district-level corpus for all two-candidate districts
## Then predict party vote with continuous predictors

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
library(spls)
#library(foreach)
#library(doMC)
library(kernlab)
library(glmnet)
library(Hmisc)
#registerDoMC(3)
library(arm)
library(gbm)
library(MASS)
library(stats)
library(polspline)
library(gam)


source("./code/twitter.R")
source("./code/build_sparse_functions.R")
# output from build.district.corpus

filename <- "tdm.sparse.20.01.aggregate.scale.quad"
load(paste("./data/", filename, ".RData",  sep=""))
corpus.district.tdm.mat <- select.tfidf(tdm.sparse, threshold=0.001)
rm(tdm.sparse)
gc()

## Take the output vector
cand.data.dist <- house.data[house.data$party=="D",
                             c("state", "district", "pctVote",
                               "d.victory", "party"),
                             
                             ]
cand.data.dist <- unique(cand.data.dist)
rm(house.data)
gc()

sort.vec <- paste(cand.data.dist$state, cand.data.dist$district,
                  sep=".")
cand.data.dist <- cand.data.dist[order(sort.vec),]
rm(sort.vec)

corpus.district.tdm.mat <-
  corpus.district.tdm.mat[order(rownames(corpus.district.tdm.mat)),]

orig.names <- colnames(corpus.district.tdm.mat)
new.names <- paste("V", 1:ncol(corpus.district.tdm.mat), sep="")
colnames(corpus.district.tdm.mat) <- new.names

corpus.district.tdm.mat.scale <- scale(corpus.district.tdm.mat)


## Define the learner library
SL.library.continuous <- c("SL.spls",
                           "SL.ridge",
                                        #"SL.randomForest", ## Doesn't improve
                                        #accuracy, takes forever to run
                           "SL.svm",
                           "SL.glmnet"
                           )

screen.lib <- c(#"screen.randomForest",
                "screen.glmnet",
                                        #"screen.randomForest",
                "All"
                )
SL.library.continuous.screen <-
  list(c("SL.bayesglm", "screen.glmnet"),
       c("SL.gbm.1", "screen.glmnet"),
       c("SL.gbm.2", "screen.glmnet"),
       c("SL.glm", "screen.glmnet"),
                                        #c("SL.gam", "screen.glmnet"),
       c("SL.polymars","screen.glmnet"),
       c("SL.step", "screen.glmnet"),
       "SL.spls",
       c("SL.ridge", screen.lib),
       c("SL.svm", screen.lib),
       c("SL.glmnet", screen.lib),
       "SL.randomForest"
       )

## SL.library.continuous.screen <- list()



is.var.constant <- function(x){
  x <- var(x, na.rm=TRUE)
  out <- x == 0
  return(out)
}

calc.rmse <- function(x, y){

  out <- sqrt(mean((x-y)^2))
  return(out)

}

## Run the loop
N <- 2
counter <- 0
set.seed(12323)
binary.list.out <- list()
rmse.list.out <- list()
while(counter <= N)
  {

    n.train <- ceiling(0.8 * dim(corpus.district.tdm.mat)[1])
    sample.vec <- sample(1:dim(corpus.district.tdm.mat)[1], n.train)
    mat.train <- corpus.district.tdm.mat.scale[sample.vec,]
    mat.test <- corpus.district.tdm.mat.scale[-sample.vec,]

    out.train.cont <- cand.data.dist$pctVote[sample.vec]

    out.test.cont <- cand.data.dist$pctVote[-sample.vec]

    data.train <- as.data.frame(mat.train)
    data.test <- as.data.frame(mat.test)
    rm(mat.train,
       mat.test)
    gc()
    gc()

    test.train.out <- is.var.constant(out.train.cont)
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


    ## Train the superlearner
    train.superlearner.cont <- SuperLearner(Y=as.numeric(out.train.cont),
                                            X=data.train,
                                            newX=data.train,
                                            SL.library=SL.library.continuous.screen,
                                            V=15,
                                            family=gaussian(),
                                            verbose=TRUE,
                                            shuffle=FALSE
                                            )

    ## Generate training sample predictions
    SL.predict.cont <- predict(train.superlearner.cont)

    ## Stack the columns
    df.plot <- data.frame(SL.predict.cont, out.train.cont)
    names(df.plot) <- c("predict", "actual")

    ## Generate the out-of-sample predictions
    predict.oos <- predict(train.superlearner.cont,
                           newdata = data.test
                           )
    df.plot.oos <- data.frame(predict.oos$fit, out.test.cont)
    names(df.plot.oos) <- c("predict", "actual")

    ## Generate RMSE errors
    rmse.train <- calc.rmse(df.plot$predict,
                            df.plot$actual
                            )

    rmse.test <- calc.rmse(df.plot.oos$predict,
                           df.plot.oos$actual
                           )

    rmse.list.out[[counter]] <- c(rmse.train,
                                  rmse.test
                                  )
    
    ## Generate binary win/loss predictions
    train.predict.win.loss <-
      df.plot$predict > 50 & cand.data.dist$party[sample.vec] == "D"
    train.actual.win.loss <- cand.data.dist$d.victory[sample.vec]

    test.predict.win.loss <-
      df.plot.oos$predict > 50 & cand.data.dist$party[-sample.vec] == "D"
    test.actual.win.loss <- cand.data.dist$d.victory[-sample.vec]

    tab.train.win.loss <- table(train.predict.win.loss,
                                train.actual.win.loss
                                )

    tab.test.win.loss <- table(test.predict.win.loss,
                               test.actual.win.loss
                               )

    binary.accuracy.train <-
      sum(diag(tab.train.win.loss)) /
        sum(tab.train.win.loss)

    binary.accuracy.test <-
      sum(diag(tab.test.win.loss)) /
        sum(tab.test.win.loss)

    binary.list.out[[counter]] <- c(binary.accuracy.train,
                                    binary.accuracy.test
                                    )

    print(paste("Finished with run", counter))

    
  } ## End while loop
## Save data and exit

binary.accuracy.rates <- sapply(binary.list.out, function(x){x})
print(dim(binary.accuracy.rates))

rmse.accuracy.rates <- sapply(rmse.list.out, function(x){x})
print(dim(rmse.accuracy.rates))


mean.binary.accuracy.rate <- rowMeans(binary.accuracy.rates)
sd.binary.accuracy.rate <- c(sd(binary.accuracy.rates[1,]),
                             sd(binary.accuracy.rates[2,])
                             )


mean.rmse.accuracy.rate <- rowMeans(binary.accuracy.rates)
sd.rmse.accuracy.rate <- c(sd(binary.accuracy.rates[1,]),
                             sd(binary.accuracy.rates[2,])
                             )

print("Mean binary accuracy rate")
print(mean.binary.accuracy.rate)
print("SD binary accuracy rates")
print(sd.binary.accuracy.rate)

pdf("./plots/bs_accuracy_density_pdf")
plot(density(binary.accuracy.rates[2,]),
     xlab="OOS accuracy rate",
     ylab="",
     main="Distribution of OOS accuracy, N=500"
     )
dev.off()

pdf("./plots/rmse_accuracy_density_pdf")
plot(density(rmse.accuracy.rates[2,]),
     xlab="OOS accuracy rate",
     ylab="",
     main="Distribution of OOS accuracy, N=500"
     )
dev.off()



## Save the entire image and exit
file.out <- paste("./data/",
                  "district.superlearner.continuous.",
                  filename,
                  ".tfidf.oosbootstrap",
                  ".RData",
                  sep=""
                  )
save.image(file.out)




