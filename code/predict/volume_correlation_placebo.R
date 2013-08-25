library(ggplot2)
library(reshape)
library(gdata)

library(plyr)
cor.breaks <- function(df, source.var="X2012.11.06", target.var="r_vote_2012"){

  c.2012 <- cor.test(df[, source.var],
                     df[, target.var]
                     )
  return(c(c.2012$estimate, c.2012$conf.int, nrow(df)))

}


winloss.breaks <- function(df, source.var="X2012.11.06", target.var="r_vote_2012"){

  source.winrate <- df[, source.var] >= 50
  target.winrate <- df[, target.var] < 50

  winrate.test <- na.omit(source.winrate == target.winrate)
  out = sum(winrate.test) / length(winrate.test)

  n <- length(winrate.test)

  return(c(out, n))

}



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

corr.volumes.2012.2012 <- cor.test(test.2012$r.ratio, test.2012$r_vote_2012)
corr.volumes.2010.2012 <- cor.test(test.2012$r.ratio, test.2012$r_vote_2010)

corr.volumes.2010.2010 <- cor.test(test.2010$r.ratio, test.2010$r_vote_2010)
corr.volumes.2008.2010 <- cor.test(test.2010$r.ratio, test.2010$r_vote_2008)

val.corr <- c(corr.volumes.2012.2012$estimate,
              corr.volumes.2010.2012$estimate,
              corr.volumes.2010.2010$estimate,
              corr.volumes.2008.2010$estimate
              )
val.ci <- rbind(corr.volumes.2012.2012$conf.int,
                corr.volumes.2010.2012$conf.int,
                corr.volumes.2010.2010$conf.int,
                corr.volumes.2008.2010$conf.int
                )

val.data.frame <- data.frame(cbind(val.corr, val.ci))
names(val.data.frame) <- c("val.corr", "ci.min", "ci.max")
val.data.frame$data.year <- c("2012 campaign data", "2012 campaign data",
                              "2010 campaign data", "2010 campaign data"
                              )
val.data.frame$vote.year <- c("Current", "Prior", "Current", "Prior")

plot.corr.placebos <- ggplot(val.data.frame,
                             aes(x=factor(vote.year),
                                 y=val.corr,
                                 ymin=ci.min,
                                 ymax=ci.max
                                 )
                             ) +
  geom_point() +
  geom_linerange() +
  scale_x_discrete("Vote year") +
  scale_y_continuous("Vote - volume correlation") +
  scale_linetype("Twitter data year") +
  facet_grid(data.year ~ .) +
  theme_bw()
print(plot.corr.placebos)
ggsave(plot.corr.placebos,
       file="../../figures/plot_twitter_volume_ratio_placebo.pdf"
       )

## Break it down by race competitiveness
corr.series.2012.2012 <- ddply(na.omit(test.2012[, c("vote.cut.2012", "r_vote_2012", "r.ratio")]),
                               .variables="vote.cut.2012",
                               .fun=cor.breaks,
                               target.var='r_vote_2012',
                               source.var="r.ratio"
                               )

corr.series.2010.2012 <- ddply(na.omit(test.2012[, c("vote.cut.2010", "r_vote_2010", "r.ratio")]),
                               .variables="vote.cut.2010",
                               .fun=cor.breaks,
                               target.var="r_vote_2010",
                               source.var="r.ratio"
                               )

corr.series.2008.2010 <- ddply(na.omit(test.2010[,c("vote.cut.2008", "r_vote_2008", "r.ratio")]),
                               .variables="vote.cut.2008",
                               .fun=cor.breaks,
                               target.var="r_vote_2008",
                               source.var="r.ratio"
                               )

corr.series.2010.2010 <- ddply(na.omit(test.2010[, c("vote.cut.2010", "r_vote_2010", "r.ratio")]),
                               .variables="vote.cut.2010",
                               .fun=cor.breaks,
                               target.var="r_vote_2010",
                               source.var="r.ratio"
                               )

names(corr.series.2012.2012) <- names(corr.series.2010.2012) <-
  names(corr.series.2010.2010) <- names(corr.series.2008.2010) <-
  c("vote.range", "estimate", "ci.min", "ci.max", "n")

corr.series.2012.2012$vote.year <- "Current"
corr.series.2010.2012$vote.year <- "Prior"
corr.series.2010.2010$vote.year <- "Current"
corr.series.2008.2010$vote.year <- "Prior"

corr.series.2012.2012$data.year <- "2012 campaign data"
corr.series.2010.2012$data.year <- "2012 campaign data"
corr.series.2010.2010$data.year <- "2010 campaign data"
corr.series.2008.2010$data.year <- "2010 campaign data"

df <- rbind(corr.series.2012.2012,
            corr.series.2010.2012,
            corr.series.2010.2010,
            corr.series.2008.2010
            )

plot.corr.series <- ggplot(df,
                           aes(x=vote.range,
                               y=estimate,
                               ymin=ci.min,
                               ymax=ci.max,
                               shape=factor(vote.year)
                               )
                           ) +
  geom_point(aes(size=n), position=position_dodge(width=0.2)) +
  geom_linerange(position=position_dodge(width=0.2)) +
  facet_grid(data.year ~ .) +
  scale_size("District count") +
  scale_shape("Election") +
  scale_x_discrete("Republican vote share") +
  scale_y_continuous("Correlation btwn. vote share and volume ratio") +
  theme_bw()
print(plot.corr.series)
ggsave(plot.corr.series,
       file="../../figures/plot_volume_correlation_placebo.pdf"
       )
