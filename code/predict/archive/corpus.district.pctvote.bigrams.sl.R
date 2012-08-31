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
library(tm)
library(RWeka)
library(ggplot2)
library(spls)
library(foreach)
library(doMC)
library(kernlab)
library(glmnet)
library(Hmisc)
#registerDoMC(3)

source("./code/twitter.R")
# output from build.district.corpus

filename <- "tdm.sparse.20.03.aggregate"
load(paste("./data/", filename, ".RData",  sep=""))
corpus.district.tdm.mat <- tdm.sparse
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

## Create the train and test datasets
set.seed(82345)
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

## Do columns in the training data have zero variance?
var.cols.train <- sapply(1:dim(data.train)[2], function(x){

  var.col <- var(data.train[,x])
  
  if(var.col == 0)
    {
      out <- TRUE
    }else{
      out <- FALSE
    }

  return(out)
  
}
                         )

data.train <- data.train[,!var.cols.train]
data.test <- data.test[,!var.cols.train]

names.orig <- names(data.train)

names(data.train) <- names(data.test) <- paste("V",
                                               1:length(names(data.train)),
                                               sep=""
                                               )

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

## Train the superlearner
train.superlearner.cont <- SuperLearner(Y=as.numeric(out.train.cont),
                                        X=data.train,
                                        newX=data.train,
                                        SL.library=SL.library.continuous.screen,
                                        V=10,
                                        family=gaussian(),
                                        verbose=TRUE,
                                        shuffle=FALSE
                                        )

## Generate training sample predictions
SL.predict.cont <- predict(train.superlearner.cont)

## Stack the columns
df.plot <- data.frame(SL.predict.cont, out.train.cont)
names(df.plot) <- c("predict", "actual")

## Generate the summed squared error for the preditions
sq.err.train <- sum((df.plot$predict - df.plot$actual)^2)
print(paste("Summed squared error for prediction:",
            sq.err.train
            )
      )

## Plot the predicted vs. actual 
svm.sl.continuous.predict <- ggplot(df.plot,
                                    aes(x=actual,
                                        y=predict
                                        )
                                    )

svm.sl.continuous.predict <- svm.sl.continuous.predict +
  geom_point() + geom_abline(intercept = 0, slope = 1, colour="red") +
  stat_smooth(method="lm") + scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100))

pdf("./plots/ggplot_sl_continuous.pdf")
print(svm.sl.continuous.predict)
dev.off()

## Generate the out-of-sample predictions
predict.oos <- predict(train.superlearner.cont,
                       newdata = data.test
                       )

sq.err.test <- sum((out.test.cont - predict.oos$fit)^2)
print(paste("Summed squared error for test prediction:",
            sq.err.test
            )
      )

## Plot the test predictions vs. actuals
df.plot.oos <- data.frame(predict.oos$fit, out.test.cont)
names(df.plot.oos) <- c("predict", "actual")

svm.sl.continuous.predict.oos <- ggplot(df.plot.oos,
                                    aes(x=actual,
                                        y=predict
                                        )
                                    )

svm.sl.continuous.predict.oos <- svm.sl.continuous.predict.oos +
  geom_point() + geom_abline(intercept = 0, slope = 1, colour="red") +
  stat_smooth(method="lm") + scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100))

pdf("./plots/ggplot_sl_continuous_oos.pdf")
print(svm.sl.continuous.predict.oos)
dev.off()

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

## cont.model.weights <- data.frame(names(train.superlearner.cont$coef),
##                                  rep("Yes", length(train.superlearner.cont$coef)),
##                                  train.superlearner.cont$coef,
##                                  train.superlearner.cont$cv.risk
##                                  )

## names(cont.model.weights) <- c("Algorithm",
##                                "Pre-screened",
##                                "Model Weighting",
##                                "Risk"
##                                )

## cont.model.weights[,3] <- round(cont.model.weights[,3], 3)
## cont.model.weights[,4] <- round(cont.model.weights[,4], 3)

## cont.model.weights[,1] <- c("Bayesian regression",
##                             "Gradiant boosting (Depth=1)",
##                             "Gradiant boosting (Depth=2)",
##                             "Linear regression",
##                             "Gen. additive models",
##                             "Polynomial splines",
##                             "Step regression",
##                             "Sparse partial least squares",
##                             "Sparse partial least squares",
##                             "Ridge regression",
##                             "Ridge regression",
##                             "SVM",
##                             "SVM",
##                             "Lasso",
##                             "Lasso",
##                             "Random Forest"
##                             )

## cont.model.weights[,2] <- c("Yes",
##                             "Yes",
##                             "Yes",
##                             "Yes",
##                             "Yes",
##                             "Yes",
##                             "Yes",
##                             "Yes",
##                             "No",
##                             "Yes",
##                             "No",
##                             "Yes",
##                             "No",
##                             "Yes",
##                             "No",
##                             "No"
##                             )

## cont.model.weights.tex <- latex(cont.model.weights,
##                                 file="./tables/cont.model.weights.tex",
##                                 label="model-weights-pctvote.tab",
##                                 rowname=NULL,
##                                 caption="Algorithm weightings for the continuous prediction output from SuperLearner",
                                
##                                 )

## Save data and exit

## Save the entire image and exit
file.out <- paste("./data/",
                  "district.superlearner.continuous.",
                  filename,
                  ".RData",
                  sep=""
                  )
save.image(file.out)




