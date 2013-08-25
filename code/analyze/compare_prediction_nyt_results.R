library(rjson)
library(RCurl)
library(foreach)
library(ggplot2)
library(reshape)
library(plyr)
cor.fun <- function(x) cor(x$vote_pct, x$dem_vote_share, use="pairwise.complete.obs")
tab.fun <- function(x){

  tab.winloss <- sum((x$binary.d.win==1 & x$vote_pct > 50) |
                     (x$binary.d.win==0 & x$vote_pct < 50),
                     na.rm=TRUE
                     )
  tab.voteshare <- sum((x$dem_vote_share > 50 & x$vote_pct > 50) |
                       (x$dem_vote_share < 50 & x$vote_pct < 50),
                       na.rm=TRUE
                       )

  vs.out <- tab.voteshare / nrow(x)
  wl.out <- tab.winloss / nrow(x)
  N <- nrow(x)
  return(c(vs.out, wl.out, N))

}

mae.fun <- function(x){

  e <- abs(x$dem_vote_share - x$vote_pct)
  e.scaled <- e / abs(x$vote_pct - 50)
  mae <- mean(abs(e), na.rm=TRUE)
  mae.scaled <- mean(abs(e.scaled), na.rm=TRUE)
  mae.quantile <- quantile(abs(e.scaled), c(0.025, 0.975), na.rm=TRUE)
  return(c(mae, mae.scaled, mae.quantile))

}

house.results.2010 <- read.csv("./results/house_results_2010.csv")
house.results.2010$year <- 2010
house.results.2012 <- read.csv("./results/house_results_2012.csv")
house.results.2012$year <- 2012
house.results.2012$party <- substr(house.results.2012$party_id, 1, 1)

house.results.2010 <- house.results.2010[,c("state_dist", "party", "pctVote", "year")]
names(house.results.2010) <- c("state_dist", "party", "vote_pct", "year")
house.results.2012 <- house.results.2012[,c("state_dist", "party", "vote_pct", "year")]


house.results <- rbind(house.results.2010,
                       house.results.2012
                       )

voteshare.2010 <- read.csv("./predictions/vote_share/continuous.prediction.master.2010.csv")
voteshare.2012 <- read.csv("./predictions/vote_share/continuous.prediction.master.2012.csv")
voteshare.2010$year <- 2010
voteshare.2012$year <- 2012

winloss.2010 <- read.csv("./predictions/win_loss/binary.prediction.master.2010.csv")
winloss.2012 <- read.csv("./predictions/win_loss/binary.prediction.master.2012.csv")
winloss.2010$year <- 2010
winloss.2012$year <- 2012

winloss <- rbind(winloss.2010,
                 winloss.2012
                 )


## Subset the vote data to Democratic candidate voteshares
house.votes <- house.results[house.results$party=="D",
                             c("state_dist",
                               "vote_pct",
                               "year"
                               )
                             ]

house.outcomes <- rbind(voteshare.2010,
                        voteshare.2012
                        )
house.outcomes <- merge(house.outcomes,
                        winloss,
                        by.x=c("state_district", "prediction.date", "year"),
                        by.y=c("state_district", "prediction.date", "year"),
                        all=TRUE
                        )

## Load the incumbency data
inc.all <- read.csv("./data/district_incumbency.csv")
house.votes <- merge(house.votes,
                     inc.all[,c("year", "district", "incumbent_party")],
                     by.x=c("year", "state_dist"),
                     by.y=c("year", "district"),
                     all=FALSE
                     )
## Compute the incumbent win rates
fun.inc.win <- function(x){

  d.inc.win <- (x$vote_pct >= 50) & (x$incumbent_party == "D")
  r.inc.win <- (x$vote_pct < 50) & (x$incumbent_party == "R")
  d.inc <- sum(x$incumbent_party=="D")
  r.inc <- sum(x$incumbent_party=="R")
  inc.win.rate <- (sum(d.inc.win) + sum(r.inc.win)) / (d.inc + r.inc)
  #d.win.rate <- sum(d.inc.win) / sum(x$incumbent_party == "D")# %in% c("D", "R"))
  return(inc.win.rate)

}

inc.win.rate <- ddply(house.votes,
                      c("year"),
                      fun.inc.win
                      )
names(inc.win.rate) <- c("year", "inc.win.rate")

inc.win.rate.byparty <- ddply(house.votes[house.votes$incumbent_party!="O",],
                              c("year", "incumbent_party"),
                              fun.inc.win
                              )

## Merge the voteshare and prediction data
house.outcomes <- merge(house.votes,
                        house.outcomes,
                        by.x=c("state_dist", "year"),
                        by.y=c("state_district", "year"),
                        all=FALSE
                        )

house.outcomes$vote_pct <-
  as.numeric(as.character(house.outcomes$vote_pct))

house.outcomes$prediction.date <- as.POSIXlt(as.character(house.outcomes$prediction.date),
                                             format="%Y-%m-%d"
                                             )




plot.vote.bydate <- ggplot(house.outcomes,
                           aes(x=as.numeric(as.character(vote_pct)),
                               y=dem_vote_share,
                               group=prediction.date,
                               colour=prediction.date
                               )
                           ) +
  geom_point(alpha=0.05) +
  stat_smooth(method="lm", alpha=0.1) +
  facet_wrap(~ year) +
  scale_x_continuous(limits=c(0,100))
print(plot.vote.bydate)
ggsave("./figures/voteshare_prediction_corr_bydate.pdf", plot.vote.bydate)



## Check the correlation between predicted and actual voteshare
## by date for the entire prediction period. Plot the output

ply.fun <- function(x){

  x1 <- cor.fun(x)
  x2 <- tab.fun(x)
  return(c(x1, x2))
}

df.voteshare.corr <- ddply(.data=house.outcomes, .variables=c("prediction.date"), .fun=ply.fun)
names(df.voteshare.corr) <- c("prediction.date",
                              "Voteshare Correlation",
                              "Voteshare Prediction Accuracy",
                              "Winloss Prediction Accuracy",
                              "N"
                              )
df.voteshare.corr$year <- format(df.voteshare.corr$prediction.date, "%Y")
df.voteshare.corr <- melt(df.voteshare.corr,
                          id.vars <- c("prediction.date", "N", "year")
                          )


plot.voteshare.corr <- ggplot(df.voteshare.corr,
                              aes(x=yday(prediction.date),
                                  y=value,
                                  group=variable,
                                  linetype=variable
                                  )
                              ) +
  geom_line() +
  facet_grid(year ~ ., scales="free_x") +
  geom_hline(data=inc.win.rate,
             aes(yintercept=inc.win.rate),
             linetype=4
             ) +
  scale_x_continuous("Prediction date (day of year)") +
  scale_y_continuous("Correlation between predicted\nand actual Democratic voteshare",
                     limits=c(0.4, 1)
                     )+
  scale_linetype("Correspondence between\npredictions and outcomes",
                 guide=guide_legend(nrow=2)
                 ) +
  scale_size(range=c(0.5, 3)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=-90, hjust=0),
        strip.text.y=element_text(angle=0),
        legend.position="top")
print(plot.voteshare.corr)
ggsave(file="./figures/voteshare_winloss_correlation_bydate.pdf",
       plot=plot.voteshare.corr,
       width=7,
       height=4
       )


final.prediction <- house.outcomes[house.outcomes$prediction.date %in% c(as.POSIXlt("2012-11-06"),
                                                                         as.POSIXlt("2010-11-02")
                                                                         ),
                                   ]

final.prediction$party.winner <-
  ifelse(final.prediction$vote_pct >= 50, "Democrat", "Republican")

plot.final.prediction <- ggplot(final.prediction,
                                aes(x=vote_pct,
                                    y=dem_vote_share,
                                    label=state_dist
                                    )
                                ) +
  geom_smooth(method="lm", colour="black") +
  geom_text(aes(colour=party.winner), size=3) +
  facet_wrap(~ year) +
  geom_abline(intercept=0, slope=1, linetype=2) +
  scale_colour_manual("Election winner", values=c(Democrat="blue", Republican="red")) +
  scale_x_continuous("Actual Democratic vote share", limits=c(0,100)) +
  scale_y_continuous("Predicted Democratic vote share on election day", limits=c(0, 100))+
  theme_bw()

print(plot.final.prediction)
ggsave(file="./figures/plot_corr_final_prediction_actual.pdf",
       plot=plot.final.prediction
       )

test <- ddply(final.prediction, .variables=c("year", "incumbent_party"), .fun=tab.fun)
names(test) <- c("Year", "Incumbent party", "Voteshare accuracy", "Win-loss accuracy", "N")
test$Year <- as.character(test$Year)

test <- merge(test,
              inc.win.rate.byparty,
              by.x=c("Year", "Incumbent party"),
              by.y=c("year", "incumbent_party"),
              all=TRUE
              )
names(test)[ncol(test)] <- "Incumbent win rate"
xtab.test <- xtable(test,
                    digits=c(0, 0, 0, 2, 2, 0, 2),
                    label="tab:accuracy-by-incumbency",
                    caption="Predictive accuracy by election and district incumbent."
                    )
print.xtable(xtab.test,
             file="./tables/predictive_accuracy_election_incumbent.tex",
             include.rownames=FALSE
             )

## And break down accuracy by the vote quantile
final.prediction$vote.quantile <- as.character(cut(final.prediction$vote_pct,
                                                   breaks=c(0, 20, 30, 40, 45, 49, 51, 55, 60, 70, 80, 100),
                                                   include.lowest=TRUE
                                                   )
                                               )

blah <- split(final.prediction,
              list(final.prediction$vote.quantile,
                   final.prediction$incumbent_party,
                   final.prediction$year
                   )
              )

accuracy.by.quantile <- ddply(final.prediction[,c("year",
                                                  "vote.quantile",
                                                  "vote_pct",
                                                  "binary.d.win",
                                                  "dem_vote_share",
                                                  "incumbent_party"
                                                  )
                                               ],
                              .variables=c("year", "vote.quantile", "incumbent_party"),
                              .fun=tab.fun
                              )

## This appears to work, but need to check by hand.
names(accuracy.by.quantile) <- c("Year",
                                 "Dem. vote share",
                                 "Incumbent",
                                 "Voteshare accuracy",
                                 "Win-loss accuracy",
                                 "N"
                                 )
accuracy.by.quantile <- melt(accuracy.by.quantile,
                             id.vars=c("Year", "Dem. vote share", "Incumbent", "N")
                             )

plot.accuracy.by.quantile <- ggplot(accuracy.by.quantile,
                                    aes(x=`Dem. vote share`,
                                        y=value,
                                        group=variable,
                                        shape=variable## ,
                                        ## size=N
                                        )
                                    ) +
  geom_point(position=position_dodge(width=0.75, height=0)) +
  facet_grid(Incumbent ~ Year) +
  scale_shape("Predictor") +
  ## scale_size("District count") +
  scale_x_discrete("Democratic vote share") +
  scale_y_continuous("Share of correctly-forecast districts") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=-90, hjust=0))

print(plot.accuracy.by.quantile)
ggsave(plot.accuracy.by.quantile,
       file="./figures/plot_accuracy_by_quantile.pdf",
       width=8,
       height=4
       )

mae.by.quantile <- ddply(final.prediction[,c("year",
                                             "vote.quantile",
                                             "vote_pct",
                                             "dem_vote_share",
                                             "incumbent_party"
                                             )
                                          ],
                         .variables=c("year", "vote.quantile", "incumbent_party"),
                         .fun=mae.fun
                         )

final.prediction$abs.error <-
  abs(final.prediction$vote_pct - final.prediction$dem_vote_share)

final.prediction$abs.error.norm <-
  final.prediction$abs.error / abs(final.prediction$vote_pct - 50)
## This appears to work, but need to check by hand.
names(mae.by.quantile) <- c("Year",
                            "Dem. vote share",
                            "Incumbent",
                            "Mean Absolute Error",
                            "ci2.5",
                            "ci97.5"
                            )


plot.mae.by.quantile <- ggplot(mae.by.quantile,
                               aes(x=`Dem. vote share`,
                                   y=`Mean Absolute Error`,
                                   ymin=ci2.5,
                                   ymax=ci97.5
                                   )
                               ) +
  geom_pointrange() +
  facet_grid(Incumbent ~ Year) +
  scale_x_discrete("Democratic vote share") +
  scale_y_continuous("Mean absolute error") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=-90, hjust=0))

print(plot.mae.by.quantile)
ggsave(plot.mae.by.quantile,
       file="./figures/plot_mae_by_quantile.pdf",
       width=8,
       height=4
       )


plot.ae.by.quantile <- ggplot(final.prediction,
                               aes(x=vote.quantile,
                                   y=abs.error.norm
                                   )
                               ) +
  geom_jitter(size=1.5) +
  facet_grid(incumbent_party ~ year) +
  scale_x_discrete("Democratic vote share") +
  scale_y_log10("Absolute error") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=-90, hjust=0))

print(plot.ae.by.quantile)
ggsave(plot.ae.by.quantile,
       file="./figures/plot_ae_by_quantile.pdf",
       width=8,
       height=4
       )
