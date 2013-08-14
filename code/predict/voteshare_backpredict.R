library(ggplot2)
library(reshape)
library(gdata)

df.sentiment.2012 <- read.csv("../../data/r_sentiment_bydistrict_2012.csv")
df.sentiment.2010 <- read.csv("../../data/r_sentiment_bydistrict_2010.csv")

df.predict.2010 <- read.csv("../../predictions/vote_share/continuous.prediction.master.wide.2010.csv")
df.results.2010 <- read.csv("../../results/map_2008_2010_results.csv")


df.results.2010 <- df.results.2010[df.results.2010$state_dist %in% df.predict.2010$state_district,]
df.predict.2010 <- df.predict.2010[df.predict.2010$state_district %in% df.results.2010$state_dist,]
df.results.2010 <- df.results.2010[df.results.2010$state_dist %in% df.predict.2010$state_district,]

df.results.2010 <- drop.levels(df.results.2010)
df.predict.2010 <- drop.levels(df.predict.2010)

district.dupes <- table(df.results.2010$state_dist)
district.dupes <- district.dupes[district.dupes==1]

df.predict.2010 <- df.predict.2010[df.predict.2010$state_district %in% names(district.dupes),]
df.results.2010 <- df.results.2010[df.results.2010$state_dist %in% names(district.dupes),]

df.predict.2010 <- df.predict.2010[order(df.predict.2010$state_district),]
df.results.2010 <- df.results.2010[order(df.results.2010$state_dist),]


lm.series.full <- sapply(2:ncol(df.predict.2010), function(x){

  d_vote_prior= 100 - df.results.2010$r_vote_2008
  d_vote_current = 100 - df.results.2010$r_vote_2010
  lm.trial <- lm(d_vote_prior ~ d_vote_current + df.predict.2010[,x])
  lm.predict <- predict(lm.trial)

  rmse <- sqrt(mean((lm.predict - d_vote_current[!is.na(df.predict.2010[,x])])^2))
  return(rmse)

})


lm.series.notwitter <- sapply(2:ncol(df.predict.2010), function(x){

  d_vote_prior= 100 - df.results.2010$r_vote_2008
  d_vote_current = 100 - df.results.2010$r_vote_2010

  lm.trial <- lm(d_vote_prior ~ d_vote_current)
  lm.predict <- predict(lm.trial)

  print(length(lm.predict))
  rmse <- sqrt(mean((lm.predict - d_vote_current)^2))
  return(rmse)

})


lm.series.novote <- sapply(2:ncol(df.predict.2010), function(x){

  d_vote = 100 - df.results.2010$r_vote_2008

  rmse <- sqrt(mean((df.predict.2010[,x] - d_vote), na.rm=TRUE)^2)
  return(rmse)

})

df.series.2010 <- data.frame(lm.series.full, lm.series.novote, lm.series.notwitter)
names(df.series.2010) <- c("lm", "novote", "notwitter")
df.series.2010$date <- 1:nrow(df.series.2010)
df.series.2010.melt <- melt(df.series.2010, id.vars="date")

plot.df.2010 <- ggplot(df.series.2010.melt,
                       aes(x=date,
                           y=value,
                           colour=variable
                           )
                       ) +
  geom_line()
print(plot.df.2010)



## Fit the full model and then use it to forecast 2012

d.vote.current = 100 - df.results$r_vote_2010
d.vote.prior = 100 - df.results$r_vote_2008
twitter.forecast = df.predict$X2010.11.02
input.df = data.frame(d.vote.current, d.vote.prior, twitter.forecast)
names(input.df) <- c("d.vote.current", "d.vote.prior", "twitter.forecast")
input.df <- na.omit(input.df)

lm.full = lm(d.vote.prior ~ d.vote.current + twitter.forecast, data=input.df)

df.predict.2012 <- read.csv("../../predictions/vote_share/continuous.prediction.master.wide.2012.csv")
df.results.2012 <- read.csv("../../results/map_2010_2012_results.csv")

df.results.2012 <- df.results.2012[df.results.2012$state_dist %in% df.predict.2012$state_district,]
df.predict.2012 <- df.predict.2012[df.predict.2012$state_district %in% df.results.2012$state_dist,]
df.results.2012 <- df.results.2012[df.results.2012$state_dist %in% df.predict.2012$state_district,]

df.results.2012 <- drop.levels(df.results.2012)
df.predict.2012 <- drop.levels(df.predict.2012)

district.dupes <- table(df.results.2012$state_dist)
district.dupes <- district.dupes[district.dupes==1]

df.predict.2012 <- df.predict.2012[df.predict.2012$state_district %in% names(district.dupes),]
df.results.2012 <- df.results.2012[df.results.2012$state_dist %in% names(district.dupes),]

df.predict.2012 <- df.predict.2012[order(df.predict.2012$state_district),]
df.results.2012 <- df.results.2012[order(df.results.2012$state_dist),]



lm.voteshare.full.2012 <- sapply(2:ncol(df.predict.2012), function(x){

  d_vote_current = 100 - df.results.2012$r_vote_2012
  d_vote_prior = 100 - df.results.2012$r_vote_2010
  twitter.forecast = df.predict.2012[,x]
  input.df = data.frame(d_vote_current, d_vote_prior, twitter.forecast)
  names(input.df) <- c("d.vote.current", "d.vote.prior", "twitter.forecast")
  input.df <- na.omit(input.df)

  predict.lm <- predict(lm.full, newdata=input.df)

  N <- nrow(input.df)
  rmse.lm = sqrt(mean((input.df$d.vote.current - predict.lm)^2))
  rmse.novote = sqrt(mean((input.df$d.vote.current - input.df$twitter.forecast)^2))
  rmse.notwitter = sqrt(mean((input.df$d.vote.current - input.df$d.vote.prior)^2))
  return(c(rmse.lm, rmse.novote, rmse.notwitter, N))

})

tabulate.winloss <- function(x, y){

  tab <- table(x, y)
  pct <- sum(diag(tab)) / sum(tab)
  return(pct)

}

lm.winloss.full.2012 <- sapply(2:ncol(df.predict.2012), function(x){

  d_vote_current = 100 - df.results.2012$r_vote_2012
  d_vote_prior = 100 - df.results.2012$r_vote_2010
  twitter.forecast = df.predict.2012[,x]
  input.df = data.frame(d_vote_current, d_vote_prior, twitter.forecast)
  names(input.df) <- c("d.vote.current", "d.vote.prior", "twitter.forecast")
  input.df <- na.omit(input.df)

  predict.lm <- predict(lm.full, newdata=input.df)

  N <- nrow(input.df)
  pct.winloss.lm <- tabulate.winloss(predict.lm >= 50,
                                  input.df$d.vote.current >= 50
                                  )

  pct.winloss.notwitter <- tabulate.winloss(input.df$d.vote.prior >= 50,
                                            input.df$d.vote.current >= 50
                                            )

  pct.winloss.novote <- tabulate.winloss(input.df$d.vote.current >= 50,
                                         input.df$twitter.forecast >= 50
                                         )

  return(c(1 - pct.winloss.lm, 1 - pct.winloss.novote, 1 - pct.winloss.notwitter, N))

})

lm.voteshare.full.2012 <- data.frame(t(lm.voteshare.full.2012))
names(lm.voteshare.full.2012) <- c("lm", "novote", "notwitter", "N")
lm.voteshare.full.2012$date <- 1:nrow(lm.voteshare.full.2012)

lm.voteshare.full.2012.melt <- melt(lm.voteshare.full.2012, id.vars=c("date", "N"))

lm.winloss.full.2012 <- data.frame(t(lm.winloss.full.2012))
names(lm.winloss.full.2012) <- c("lm", "novote", "notwitter", "N")
lm.winloss.full.2012$date <- 1:nrow(lm.winloss.full.2012)

lm.winloss.full.2012.melt <- melt(lm.winloss.full.2012, id.vars=c("date", "N"))

lm.voteshare.full.2012.melt$type <- "voteshare"
lm.winloss.full.2012.melt$type <- "winloss"

df <- rbind(lm.voteshare.full.2012.melt,
            lm.winloss.full.2012.melt
            )


plot.lm.2012 <- ggplot(df,
                       aes(x=date,
                           y=value,
                           colour=variable
                           )
                       ) +
  geom_point(aes(size=N)) +
  geom_line() +
  facet_grid(type ~ ., scales="free")
print(plot.lm.2012)


## Test incumbent win rates for 2012 around the cutpoint

df.results.2012$vote.margin.2010 <- abs(50 - df.results.2012$r_vote_2010)
df.results.2010$vote.margin.2008 <- abs(50 - df.results.2010$r_vote_2008)


margins <- seq(1, 16, 1)
winloss.at.margin <- sapply(margins, function(x){

  df.test.2012 <- df.results.2012[df.results.2012$vote.margin.2010 < x,]
  pct.winloss.2012 = tabulate.winloss(df.test.2012$r_vote_2012 >= 50, df.test.2012$r_vote_2010 >= 50)

  df.test.2010 <- df.results.2010[df.results.2010$vote.margin.2008 < x,]
  pct.winloss.2010 = tabulate.winloss(df.test.2010$r_vote_2010 >= 50, df.test.2010$r_vote_2008 >= 50)

  return(c(x, pct.winloss.2012, pct.winloss.2010))

})

winloss.at.margin <- data.frame(t(winloss.at.margin))
names(winloss.at.margin) <- c("vote.margin.2010", "pct.win.2012", "pct.win.2010")
winloss.at.margin <- melt(winloss.at.margin, id.vars="vote.margin.2010")

levels(winloss.at.margin$variable) <- c("2012", "2010")

plot.vote.margin <- ggplot(winloss.at.margin,
                           aes(x=vote.margin.2010,
                               y=value,
                               colour=variable
                               )
                           ) +
  geom_line() +
  scale_colour_discrete("Election year") +
  scale_x_continuous("Absolute vote margin in prior election") +
  scale_y_continuous("Pct. districts with same win/loss outcome in current election")
print(plot.vote.margin)


df.results.2012 <- merge(df.results.2012,
                         df.sentiment.2012,
                         by.x="state_dist",
                         by.y="dist",
                         all.x=TRUE,
                         all.y=FALSE
                         )

lm.sentiment <- lm(df.results.2012$r_vote_2012 ~ df.predict.2012$X2012.11.06 +
                   df.results.2012$sentiment_ratio
                   )

lm.sentiment <- lm(df.results.2012$r_vote_2012 ~ df.predict.2012$X2012.11.06 +
                   df.results.2012$sentiment_ratio + df.results.2012$r_vote_2010
                   )

predict.sentiment <- predict(lm.sentiment)

plot(predict.sentiment ~ na.omit(df.results.2012)$r_vote_2012, xlim=c(0,100), ylim=c(0,100))

plot.sentiment.density <- ggplot(df.results.2012,
                                 aes(x=sentiment_ratio,
                                     y=r_vote_2010
                                     )
                                 ) +
  geom_point()
print(plot.sentiment.density)
