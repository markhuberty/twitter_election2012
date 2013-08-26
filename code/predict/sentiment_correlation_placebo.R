library(ggplot2)
library(reshape)
library(gdata)
library(apsrtable)
library(xtable)

## Load sentiment, results / prior results, and twitter predictions data
df.sentiment.2012 <- read.csv("../../data/r_sentiment_bydistrict_2012.csv")
df.sentiment.2010 <- read.csv("../../data/r_sentiment_bydistrict_2010.csv")

names(df.sentiment.2012)[1] <- names(df.sentiment.2010)[1] <- "state_dist"

df.results.2012 <- read.csv("../../results/map_2010_2012_results.csv")
df.results.2010 <- read.csv("../../results/map_2008_2010_results.csv")

district.dupes <- table(df.results.2010$state_dist)
district.dupes <- district.dupes[district.dupes==1]

df.results.2010 <- df.results.2010[df.results.2010$state_dist %in% names(district.dupes),]

# And repeat for the 2012 data
district.dupes <- table(df.results.2012$state_dist)
district.dupes <- district.dupes[district.dupes==1]
df.results.2012 <- df.results.2012[df.results.2012$state_dist %in% names(district.dupes),]

df.test.2012 <- merge(df.sentiment.2012[,c("state_dist", "sentiment_ratio", "sentiment_score")],
                      df.results.2012[,c("state_dist", "r_vote_2012", "r_vote_2010", "state_dist_prior")],
                      by.x="state_dist",
                      by.y="state_dist",
                      all=FALSE
                      )

df.test.2010 <- merge(df.sentiment.2010[,c("state_dist", "sentiment_ratio", "sentiment_score")],
                      df.results.2010[,c("state_dist", "r_vote_2008", "r_vote_2010")],
                      by.x="state_dist",
                      by.y="state_dist",
                      all=FALSE
                      )

vote.breaks <- c(0, 40, 45, 55, 60, 100)
df.test.2012$vote.cut.2012 <- cut(df.test.2012$r_vote_2012,
                                  breaks=vote.breaks
                                  )
df.test.2012$vote.cut.2010 <- cut(df.test.2012$r_vote_2010,
                                  breaks=vote.breaks
                                  )

df.test.2010$vote.cut.2010 <- cut(df.test.2010$r_vote_2010,
                                  breaks=vote.breaks
                                  )
df.test.2010$vote.cut.2008 <- cut(df.test.2010$r_vote_2008,
                                  breaks=vote.breaks
                                  )


names(df.test.2010) <-
  c("state_dist", "sentiment_ratio", "sentiment_score",
    "r_vote_prior", "r_vote_current", "vote_cut_current", "vote_cut_prior")
names(df.test.2012) <-   c("state_dist", "sentiment_ratio", "sentiment_score",
    "r_vote_prior", "r_vote_current", "state_dist_prior", "vote_cut_current", "vote_cut_prior")


lm.2010.2010 <- lm(r_vote_current ~ r_vote_prior + sentiment_ratio,
                   data=na.omit(df.test.2010)
                   )
lm.2010.2010.no.vote <- lm(r_vote_current ~ sentiment_ratio,
                          data=na.omit(df.test.2010)
                          )
lm.2010.2010.no.twitter <- lm(r_vote_current ~ r_vote_prior,
                             data=na.omit(df.test.2010)
                             )
tab.lm <- apsrtable(lm.2010.2010,
                    lm.2010.2010.no.vote,
                    lm.2010.2010.no.twitter,
                    model.names=c("Complete", "Twitter only", "Vote only"),
                    coef.names=c("Intercept", "Prior vote", "Sentiment"),
                    caption="Regression table for a model of form $V_{t} \\backsim V_{t-1} + Sentiment$. Current and prior vote share use the share of the two-party vote. $Sentiment = \\frac{S_R}{S_D + S_R}$, where $S_p = \\frac{pos_p}{pos_p + neg_p}$ for $p \\in \\{R, D\\}$.",
                    label="tab:lm-sentiment-reg"
                    )
writeLines(tab.lm, "../../tables/lm_sentiment_model.tex")


predict.2010 <- predict(lm.2010.2010, newdata=na.omit(df.test.2010))
predict.2010.novote <- predict(lm.2010.2010.no.vote, newdata=na.omit(df.test.2010))
predict.2010.notwitter <- predict(lm.2010.2010.no.twitter, newdata=na.omit(df.test.2010))

predict.2012 <- predict(lm.2010.2010, newdata=df.test.2012)
predict.2012.novote <- predict(lm.2010.2010.no.vote, newdata=df.test.2012)
predict.2012.notwitter <- predict(lm.2010.2010.no.twitter, newdata=df.test.2012)

voteshare.mse <- function(df, source.var, target.var){

  sq.err <- (df[, source.var] - df[, target.var])^2
  mse <- mean(sq.err)
  return(sqrt(mse))


}

compute.winloss <- function(df, source.var="X2012.11.06", target.var="r_vote_2012"){

  source.winrate <- df[, source.var] >= 50
  target.winrate <- df[, target.var] >= 50

  winrate.test <- na.omit(source.winrate == target.winrate)
  out = sum(winrate.test) / length(winrate.test)

  n <- length(winrate.test)

  return(c(out, n))

}

compute.winloss.breaks <- function(df, source.var="X2012.11.06", target.var="r_vote_2012",
                                   breaks="vote_cut_current"){

  source.winrate <- df[, source.var] >= 50
  target.winrate <- df[, target.var] >= 50

  winrate.test <- source.winrate == target.winrate
  out <- tapply(winrate.test, df[, breaks], mean, na.rm=TRUE)

  return(out)

}

inc.success <- function(df){

  is_incumbent <- df$r_vote_prior >= 50
  won_current <- df$r_vote_current >= 50

  inc_won <- is_incumbent == won_current
  return(mean(inc_won))

}

df.predict.2010 <- data.frame(na.omit(df.test.2010)$r_vote_current,
                              na.omit(df.test.2010)$vote_cut_current,
                              predict.2010,
                              predict.2010.novote,
                              predict.2010.notwitter
                              )
df.predict.2012 <- data.frame(df.test.2012$r_vote_current,
                              df.test.2012$vote_cut_current,
                              predict.2012,
                              predict.2012.novote,
                              predict.2012.notwitter
                              )

names(df.predict.2012) <- names(df.predict.2010) <- c("actual", "vote_cut_current", "predict",
                                                      "predict.novote",
                                                      "predict.notwitter"
                                                      )

pct.winloss.breaks.2010 <- compute.winloss.breaks(df.predict.2010,
                                                  "actual",
                                                  "predict"
                                                  )
pct.winloss.breaks.2012 <- compute.winloss.breaks(df.predict.2012,
                                                  "actual",
                                                  "predict"
                                                  )

pct.winloss.2010 <- compute.winloss(df.predict.2010, "actual", "predict")
pct.winloss.2012 <- compute.winloss(df.predict.2012, "actual", "predict")

pct.winloss.2010.novote <- compute.winloss(df.predict.2010, "actual", "predict.novote")
pct.winloss.2012.novote <- compute.winloss(df.predict.2012, "actual", "predict.novote")

pct.winloss.2010.notwitter <- winloss.breaks(df.predict.2010, "actual", "predict.notwitter")
pct.winloss.2012.notwitter <- winloss.breaks(df.predict.2012, "actual", "predict.notwitter")

mse.2010 <- voteshare.mse(df.predict.2010, "actual", "predict")
mse.2012 <- voteshare.mse(df.predict.2012, "actual", "predict")

mse.2010.novote <- voteshare.mse(df.predict.2010, "actual", "predict.novote")
mse.2012.novote <- voteshare.mse(df.predict.2012, "actual", "predict.novote")

mse.2010.notwitter <- voteshare.mse(df.predict.2010, "actual", "predict.notwitter")
mse.2012.notwitter <- voteshare.mse(df.predict.2012, "actual", "predict.notwitter")


inc.success.2010 <- inc.success(df.test.2010)
inc.success.2012 <- inc.success(df.test.2012)
inc.mse.2010 <- voteshare.mse(df.test.2010, "r_vote_current", "r_vote_prior")
inc.mse.2012 <- voteshare.mse(df.test.2012, "r_vote_current", "r_vote_prior")

tab.success <- data.frame(c("Full model", "Vote only", "Twitter only", "Incumbent"),
                          c(pct.winloss.2010[1], pct.winloss.2010.notwitter[1], pct.winloss.2010.novote[1],
                            inc.success.2010),
                          c(pct.winloss.2012[1], pct.winloss.2012.notwitter[1], pct.winloss.2012.novote[1],
                            inc.success.2012),
                          c(mse.2010, mse.2010.notwitter, mse.2010.novote, inc.mse.2010),
                          c(mse.2012, mse.2012.notwitter, mse.2012.novote, inc.mse.2012)
                          )

names(tab.success) <- c("Method", "W/L 2010", "W/L 2012", "MSE 2010", "MSE 2012")

xtab.success <- xtable(tab.success, caption="Predictive accuracy for sentiment-based forecasts. 2010 results reflect back-casting. 2012 results forecast using the model trained on the 2010 election. W/L refers to the binary win/loss outcome. MSE presents the mean squared error of the two-party vote share forecast.",
                       label="tab:sentiment-accuracy"
                       )
print(xtab.success,
      digits=2,
      file="../../tables/tab_sentiment_accuracy.tex",
      include.rownames=FALSE,
      caption.placement="top"
      )
## And then the placebo
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
       file="../../figures/sentiment_forecast_performance.pdf"
       )


df.placebo.fwd <- merge(df.test.2012[,c("state_dist_prior", "r_vote_current", "r_vote_prior", "sentiment_ratio"),],
                        df.test.2010[,c("state_dist", "vote_cut_current")],
                        by.x="state_dist_prior",
                        by.y="state_dist",
                        all=FALSE
                        )

df.placebo.rev <- merge(df.test.2012[,c("state_dist_prior", "sentiment_ratio"),],
                        df.test.2010[,c("state_dist", "r_vote_current", "r_vote_prior")],
                        by.x="state_dist_prior",
                        by.y="state_dist",
                        all=FALSE
                        )

df.placebo.fwd$placebo.predict.fwd <- predict(lm.2010.2010,
                                              newdata=df.placebo.fwd
                                              )
df.placebo.rev$placebo.predict.rev <- predict(lm.2010.2010,
                                              newdata=df.placebo.rev
                                              )
df.placebo.fwd$placebo.predict.fwd.not <- predict(lm.2010.2010.no.twitter,
                                                  newdata=df.placebo.fwd
                                                  )
df.placebo.rev$placebo.predict.rev.not <- predict(lm.2010.2010.no.twitter,
                                                  newdata=df.placebo.rev
                                                  )

rmse.placebo.fwd <- voteshare.mse(df.placebo.fwd,
                                  "placebo.predict.fwd",
                                  "r_vote_current"
                                  )
rmse.placebo.rev <- voteshare.mse(df.placebo.rev,
                                  "placebo.predict.rev",
                                  "r_vote_prior"
                                  )
rmse.placebo.fwd.not <- voteshare.mse(df.placebo.fwd,
                                      "placebo.predict.fwd.not",
                                      "r_vote_current"
                                      )
rmse.placebo.rev.not <- voteshare.mse(df.placebo.rev,
                                      "placebo.predict.rev.not",
                                      "r_vote_prior"
                                      )
