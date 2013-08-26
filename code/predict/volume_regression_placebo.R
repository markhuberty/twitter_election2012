library(ggplot2)
library(reshape)
library(gdata)
library(xtable)
library(plyr)
library(stargazer)

cor.breaks <- function(df, source.var="X2012.11.06", target.var="r_vote_2012"){

  c.2012 <- cor.test(df[, source.var],
                     df[, target.var]
                     )
  return(c(c.2012$estimate, c.2012$conf.int, nrow(df)))

}


winloss.breaks <- function(df, source.var="X2012.11.06", target.var="r_vote_2012"){

  source.winrate <- df[, source.var] >= 50
  target.winrate <- df[, target.var] >= 50

  winrate.test <- na.omit(source.winrate == target.winrate)
  out = sum(winrate.test) / length(winrate.test)

  n <- length(winrate.test)

  return(c(out, n))

}

boot.winloss <- function(df, n.boots){
  n.sample <- floor(0.9 * nrow(df))
  out <- sapply(1:n.boots, function(x){


    sample.vec <- sample(1:n.boots, n.sample, replace=FALSE)
    df.train <- df[sample.vec,]
    df.test <- df[-sample.vec,]

    lm.boot <- lm(vote.current ~ r.ratio, data=df.train)
    df.test$prediction <- predict(lm.boot, newdata=df.test)

    predict.success <- winloss.breaks(df.test, "vote.current", "prediction")[1]
    inc.success <- winloss.breaks(df.test, "vote.current", "vote.prior")[1]
    return(c(predict.success, inc.success))


  })
  sum.out <- rowMeans(out)
  var.out <- apply(out, 1, sd)
  return(cbind(sum.out, var.out))

}

# Define where we break the vote outcomes into categories
vote.breaks <- c(0, 40, 45, 55, 60, 100)

## Placebo test on volume ratios: correlation between volume ratio in period T
## and volume ratio in period T-1 should at least be similar, at best
## be better b/c it's just embedding incumbency

tweet.volumes.2012 <- read.csv("../../data/tweets_volumes_2012.csv")
tweet.volumes.2010 <- read.csv("../../data/tweets_volumes_2010.csv")

df.results.2012 <- read.csv("../../results/map_2010_2012_results.csv")
df.results.2010 <- read.csv("../../results/map_2008_2010_results.csv")

tweet.volumes.2012$r.ratio <- tweet.volumes.2012$R / (tweet.volumes.2012$R +
                                                      tweet.volumes.2012$D)

tweet.volumes.2010$r.ratio <- tweet.volumes.2010$R / (tweet.volumes.2010$R +
                                                      tweet.volumes.2010$D)

test.2012 <- merge(df.results.2012,
                   tweet.volumes.2012,
                   by.x="state_dist",
                   by.y="district",
                   all.x=FALSE,
                   all.y=FALSE
                   )

test.2010 <- merge(df.results.2010,
                   tweet.volumes.2010,
                   by.x="state_dist",
                   by.y="district",
                   all.x=FALSE,
                   all.y=FALSE
                   )


test.placebo <- merge(test.2010[, c("state_dist", "r_vote_2008", "r_vote_2010")],
                      test.2012[, c("state_dist_prior", "r.ratio")],
                      by.x="state_dist",
                      by.y="state_dist_prior",
                      all=FALSE
                      )
test.placebo.baseline <- test.2010[test.2010$state_dist %in% test.placebo$state_dist,]

# Break the vote outcomes into categories based on
# the competitiveness of each race
test.2012$vote.cut.2012 <- cut(test.2012$r_vote_2012,
                               breaks=vote.breaks
                               )
test.2012$vote.cut.2010 <- cut(test.2012$r_vote_2010,
                               breaks=vote.breaks
                               )
test.2010$vote.cut.2010 <- cut(test.2010$r_vote_2010,
                               breaks=vote.breaks
                               )
test.2010$vote.cut.2008 <- cut(test.2010$r_vote_2008,
                               breaks=vote.breaks
                               )

# Standardize the inputs
df.input.2010 <- test.2010[,c("r.ratio", "r_vote_2010", "r_vote_2008")]
df.input.2012 <- test.2012[,c("r.ratio", "r_vote_2012", "r_vote_2010")]
df.input.placebo <- test.placebo[,c("r.ratio", "r_vote_2010", "r_vote_2008")]
df.input.placebo.baseline <- test.placebo.baseline[, c("r.ratio", "r_vote_2010", "r_vote_2008")]

names(df.input.2010) <-
  names(df.input.2012) <-
  names(df.input.placebo) <-
  names(df.input.placebo.baseline) <-
  c("r.ratio", "vote.current", "vote.prior")


## Generate the regression for 2010 and use it to forecast 2012
lm.2010.2010 <- lm(vote.current ~ r.ratio + vote.prior, data=df.input.2010)
lm.2010.2010.no.twitter <- lm(vote.current ~ vote.prior, data=df.input.2010)
lm.2010.2010.no.vote <- lm(vote.current ~ r.ratio, data=df.input.2010)

## Generate the comparison regression for 2012
lm.2012.2012 <- lm(vote.current ~ r.ratio + vote.prior, data=df.input.2012)
lm.2012.2012.no.twitter <- lm(vote.current ~ vote.prior, data=df.input.2012)
lm.2012.2012.no.vote <- lm(vote.current ~ r.ratio, data=df.input.2012)


tab.lm <- apsrtable(lm.2010.2010,
                    lm.2010.2010.no.twitter,
                    lm.2010.2010.no.vote,
                    model.names=c("Complete", "Vote only", "Twitter only"),
                    coef.names=c("Intercept", "Twitter ratio", "Prior vote"),
                    caption="Regression table for a model of form $V_{t} \\backsim V_{t-1} + R$. Current and prior vote share use the share of the two-party vote. Twitter ratio is defined as $R = \\frac{T_R}{T_R + T_D}$ for Twitter message volumes $T_p, p \\in {Republican, Democrat}$.",
                    label="tab:lm-volume-reg"
                    )
writeLines(tab.lm, "../../tables/lm_volume_model.tex")


## Predict the 2012 election given the model from 2010
df.input.2012$prediction <- predict(lm.2010.2010, newdata=df.input.2012)
df.input.2012$prediction.novote <- predict(lm.2010.2010.no.vote, newdata=df.input.2012)
df.input.2012$prediction.notwitter <- predict(lm.2010.2010.no.twitter, newdata=df.input.2012)

## Predict the 2010 election from the model
df.input.2010$prediction <- predict(lm.2010.2010)
df.input.2010$prediction.novote <- predict(lm.2010.2010.no.vote)
df.input.2010$prediction.notwitter <- predict(lm.2010.2010.no.twitter)


## Predict the 2010 election from the 2012 ratio data,
## and establish the valid baseline w/ the same set of races
## using the 2010 ratio data
df.input.placebo$prediction <- predict(lm.2010.2010, newdata=df.input.placebo)
df.input.placebo.baseline$prediction <- predict(lm.2010.2010,
                                                newdata=df.input.placebo.baseline
                                                )

## Break down performance by race competitiveness
df.input.2012$vote.breaks <- cut(df.input.2012$vote.current,
                                 vote.breaks
                                 )
df.input.2010$vote.breaks <- cut(df.input.2010$vote.current,
                                 vote.breaks
                                 )
df.input.placebo$vote.breaks <- cut(df.input.placebo$vote.current,
                                    vote.breaks
                                    )
df.input.placebo.baseline$vote.breaks <- cut(df.input.placebo.baseline$vote.current,
                                             vote.breaks
                                             )

## Compute accuracy for the predictor. Also compute it for the
## pure incumbency forecast
overall.accuracy.2010 <- winloss.breaks(df.input.2010, "vote.current", "prediction")
overall.accuracy.2012 <- winloss.breaks(df.input.2012, "vote.current", "prediction")

overall.accuracy.2010.novote <- winloss.breaks(df.input.2010, "vote.current", "prediction.novote")
overall.accuracy.2012.novote <- winloss.breaks(df.input.2012, "vote.current", "prediction.novote")

overall.accuracy.2010.notwitter <- winloss.breaks(df.input.2010, "vote.current", "prediction.notwitter")
overall.accuracy.2012.notwitter <- winloss.breaks(df.input.2012, "vote.current", "prediction.notwitter")



overall.accuracy.2010.placebo <- winloss.breaks(df.input.placebo, "vote.current", "prediction")
overall.accuracy.2010.placebo.baseline <- winloss.breaks(df.input.placebo.baseline,
                                                         "vote.current",
                                                         "prediction"
                                                         )

voteshare.mse <- function(df, source.var, target.var){

  sq.err <- (df[, source.var] - df[, target.var])^2
  mse <- mean(sq.err)
  return(sqrt(mse))


}

mse.2010 <- voteshare.mse(df.input.2010, "vote.current", "prediction")
mse.2012 <- voteshare.mse(df.input.2012, "vote.current", "prediction")

mse.novote.2010 <- voteshare.mse(df.input.2010, "vote.current", "prediction.novote")
mse.novote.2012 <- voteshare.mse(df.input.2012, "vote.current", "prediction.novote")

mse.notwitter.2010 <- voteshare.mse(df.input.2010, "vote.current", "prediction.notwitter")
mse.notwitter.2012 <- voteshare.mse(df.input.2012, "vote.current", "prediction.notwitter")

inc.mse.2010 <- voteshare.mse(df.input.2010, "vote.current", "vote.prior")
inc.mse.2012 <- voteshare.mse(df.input.2012, "vote.current", "vote.prior")


incumbency.rate.2010 <- winloss.breaks(df.input.2010, "vote.current", "vote.prior")
incumbency.rate.2012 <- winloss.breaks(df.input.2012, "vote.current", "vote.prior")
incumbency.rate.placebo <- winloss.breaks(df.input.placebo, "vote.current", "vote.prior")
incumbency.rate.placebo.baseline <- winloss.breaks(df.input.placebo,
                                                   "vote.current",
                                                   "vote.prior"
                                                   )

tab.success <- data.frame(c("Full model", "Vote only", "Twitter only", "Incumbent"),
                          c(overall.accuracy.2010[1], overall.accuracy.2010.notwitter[1],
                            overall.accuracy.2010.novote[1], incumbency.rate.2010[1]),
                          c(overall.accuracy.2012[1], overall.accuracy.2012.notwitter[1],
                            overall.accuracy.2012.novote[1], incumbency.rate.2012[1]),
                          c(mse.2010, mse.notwitter.2010, mse.novote.2010, inc.mse.2010),
                          c(mse.2012, mse.notwitter.2012, mse.novote.2012, inc.mse.2012)
                          )
names(tab.success) <- c("Method", "W/L 2010", "W/L 2012", "MSE 2010", "MSE 2012")

tab.success.melt <- melt(tab.success, id.vars="Method")
tab.success.melt$year <- sapply(as.character(tab.success.melt$variable), function(x){

  y <- as.integer(strsplit(x, " ")[[1]][2])

})

tab.success.melt$type <- sapply(as.character(tab.success.melt$variable), function(x){

  y <- strsplit(x, " ")[[1]][1]

  if(y=="MSE")
    return("Voteshare RMSE")
  else
    return("Win/loss accuracy")

})



plot.tab.success <- ggplot(tab.success.melt,
                           aes(x=factor(year),
                               y=value,
                               group=Method,
                               shape=Method
                               )
                           ) +
  geom_line(alpha=0.5) +
  geom_point(size=4) +
  facet_wrap(~ type, scales="free") +
  scale_x_discrete("Election year") +
  scale_y_continuous("") +
  scale_shape("Forecast model") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16),
        strip.text.x=element_text(size=16)
        )
print(plot.tab.success)
ggsave(plot.tab.success,
       width=7,
       height=7,
       file="../../figures/volume_forecast_performance.pdf"
       )


# Then generate the placebo outputs
# eventually want placebo (past), actual (future) for accuracy @
# cutpoint
# for 3 predictors. So 3 * 2 (3 models, placebo + normal forecast)
df.placebo <- data.frame(c(overall.accuracy.2012),
                         )
df.accuracy <- cbind(rbind(overall.accuracy.2010,
                           overall.accuracy.2012,
                           overall.accuracy.2010.placebo,
                           overall.accuracy.2010.placebo.baseline
                           ),
                     rbind(incumbency.rate.2010,
                           incumbency.rate.2012,
                           incumbency.rate.placebo,
                           incumbency.rate.placebo.baseline
                           )
                     )
df.accuracy <- data.frame(df.accuracy[,c(1,3,4)])
names(df.accuracy) <- c("Forecast", "Incumbency", "District Ct.")
rownames(df.accuracy) <- c("2010", "2012", "2010, placebo", "2010, baseline")

tab.volume.accuracy <- xtable(df.accuracy,
                              digits=c(0, 2, 2, 0),
                              caption="Forecast accuracy for volume ratio compared with incumbency win rates.", label="tab:volume-forecast-accuracy"
                              )
print(tab.volume.accuracy,
      file="../../tables/volume_ratio_accuracy.tex"
      )


## Then compute the winloss behavior by
## And check winloss accuracy by race competitiveness
winloss.breaks.series.2010.2010 <- ddply(na.omit(df.input.2010[, c("vote.breaks", "vote.current",
                                                                   "prediction")]),
                               .variables="vote.breaks",
                               .fun=winloss.breaks,
                               target.var='vote.current',
                               source.var="prediction"
                               )

winloss.breaks.series.2012.2012 <- ddply(na.omit(df.input.2012[, c("vote.breaks", "vote.current",
                                                                   "prediction")]),
                               .variables="vote.breaks",
                               .fun=winloss.breaks,
                               target.var='vote.current',
                               source.var="prediction"
                               )

winloss.breaks.series.placebo <- ddply(na.omit(df.input.placebo[, c("vote.breaks", "vote.current",
                                                                   "prediction")]),
                               .variables="vote.breaks",
                               .fun=winloss.breaks,
                               target.var='vote.current',
                               source.var="prediction"
                               )
winloss.breaks.series.placebo.baseline <-
  ddply(na.omit(df.input.placebo.baseline[, c("vote.breaks", "vote.current",
                                              "prediction")]),
        .variables="vote.breaks",
        .fun=winloss.breaks,
        target.var='vote.current',
        source.var="prediction"
        )

winloss.breaks.series.placebo.inc <-
  ddply(na.omit(df.input.placebo.baseline[, c("vote.breaks", "vote.current",
                                              "vote.prior")]),
        .variables="vote.breaks",
        .fun=winloss.breaks,
        target.var='vote.current',
        source.var="vote.prior"
        )

winloss.breaks.series.2012.2012.inc <- ddply(na.omit(df.input.2012[, c("vote.breaks", "vote.current",
                                                                   "vote.prior")]),
                               .variables="vote.breaks",
                               .fun=winloss.breaks,
                               target.var='vote.current',
                               source.var="vote.prior"
                               )
winloss.breaks.series.2010.2010.inc <- ddply(na.omit(df.input.2010[, c("vote.breaks", "vote.current",
                                                                   "vote.prior")]),
                               .variables="vote.breaks",
                               .fun=winloss.breaks,
                               target.var='vote.current',
                               source.var="vote.prior"
                               )

names(winloss.breaks.series.2012.2012) <-
  names(winloss.breaks.series.2010.2010) <-
  names(winloss.breaks.series.placebo) <-
  names(winloss.breaks.series.placebo.baseline) <-
  names(winloss.breaks.series.2012.2012.inc) <-
  names(winloss.breaks.series.2010.2010.inc) <-
  names(winloss.breaks.series.placebo.inc) <-
  c("vote.range", "winloss.accuracy", "n")

winloss.breaks.series.2010.2010$forecast <- "Forecast"
winloss.breaks.series.2012.2012$forecast <- "Forecast"
winloss.breaks.series.2010.2010$type <- "2010 campaign backcast"
winloss.breaks.series.2012.2012$type <- "2012 campaign forecast"

winloss.breaks.series.2012.2012.inc$forecast <- "Incumbency"
winloss.breaks.series.2010.2010.inc$forecast <- "Incumbency"
winloss.breaks.series.2012.2012.inc$type <- "2012 campaign forecast"
winloss.breaks.series.2010.2010.inc$type <- "2010 campaign backcast"
winloss.breaks.series.placebo.inc$forecast <- "Incumbency"

winloss.breaks.series.placebo$forecast <- "Placebo"
winloss.breaks.series.placebo.baseline$forecast <- "Forecast"
winloss.breaks.series.placebo$type <- "2010 campaign placebo"
winloss.breaks.series.placebo.baseline$type <- "2010 campaign placebo"
winloss.breaks.series.placebo.inc$type <- "2010 campaign placebo"

df.winloss.breaks <- rbind(winloss.breaks.series.2010.2010,
                           winloss.breaks.series.2012.2012,
                           winloss.breaks.series.2010.2010.inc,
                           winloss.breaks.series.2012.2012.inc,
                           winloss.breaks.series.placebo,
                           winloss.breaks.series.placebo.baseline,
                           winloss.breaks.series.placebo.inc
                           )

## This is almost right, still a little to hard to grok
plot.winloss.breaks <- ggplot(df.winloss.breaks,
                              aes(x=vote.range,
                                  y=winloss.accuracy,
                                  shape=factor(forecast)
                                  )
                              ) +
  geom_point(size=3, position=position_dodge(width=0.4)) +
  scale_x_discrete("Republican vote share") +
  scale_y_continuous("Win / loss predictive accuracy") +
  scale_shape("Forecast basis") +
  scale_size("District count") +
  facet_grid(. ~ type) +
  theme_bw() +
  theme(axis.text.x=element_text(size=15),
        axis.title=element_text(size=15)
        )

print(plot.winloss.breaks)
ggsave(plot.winloss.breaks,
       file="../../figures/plot_volume_regression_winloss.pdf"
       )
