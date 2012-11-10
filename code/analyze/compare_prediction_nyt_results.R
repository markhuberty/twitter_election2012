library(rjson)
library(RCurl)
library(foreach)
library(ggplot2)
library(reshape)

house.results <- read.csv("./data/house_vote_results.csv")
## Load up the prediction data
voteshare <- read.csv("~/continuous.prediction.master.wide.csv")

## Subset the vote data to Democratic candidate voteshares
house.votes <- house.results[house.results$party_bucket=="Dem" &
                             house.results$uncontested==FALSE,][c("state_dist",
                               "vote_pct_display"
                               )
                               ]

## Merge the voteshare and prediction data
house.votes <- merge(house.votes,
                     voteshare,
                     by.x="state_dist",
                     by.y="state_district",
                     all=FALSE
                     )
house.votes$vote_pct_display <-
  as.numeric(as.character(house.votes$vote_pct_display))


## Reshape and plot the relationship between actual and predicted voteshare by date
house.votes.melt <- melt(house.votes,
                         id.vars=c("state_dist", "vote_pct_display")
                         )
house.votes.melt$variable <- gsub("X", "", house.votes.melt$variable)
house.votes.melt$variable <- as.POSIXlt(house.votes.melt$variable,
                                        format="%Y.%m.%d"
                                        )


plot.vote.bydate <- ggplot(house.votes.melt,
                           aes(x=as.numeric(as.character(vote_pct_display)),
                               y=value,
                               group=variable,
                               colour=variable
                               )
                           ) +
  geom_point(alpha=0.05) +
  stat_smooth(method="lm", alpha=0.1) +
  scale_x_continuous(limits=c(0,100))
ggsave("./figures/voteshare_prediction_corr_bydate.pdf", plot.vote.bydate)



## Check the correlation between predicted and actual voteshare
## by date for the entire prediction period. Plot the output
voteshare.corr <- sapply(3:ncol(house.votes), function(x){

  cor(house.votes$vote_pct_display, house.votes[,x], use="pairwise.complete.obs")


}
                         )


accuracy.rate <- sapply(3:ncol(house.votes), function(x){

  tab.winloss <- table(house.votes$vote_pct_display > 50,
                       house.votes[,x] > 50
                       )
  out <- sum(diag(tab.winloss)) / sum(tab.winloss)
  N <- sum(tab.winloss)
  return(c(out, N))

}
                         )


## Aggregate the correlation and timewise accuracy data
## and plot it.
df.voteshare.corr <- data.frame(voteshare.corr,
                                t(accuracy.rate),
                                colnames(house.votes)[3:ncol(house.votes)]
                                )
names(df.voteshare.corr) <- c("Voteshare Correlation",
                              "Prediction Accuracy",
                              "N",
                              "date"
                              )
df.voteshare.corr$date <- gsub("X", "", df.voteshare.corr$date)
df.voteshare.corr$date <- as.POSIXlt(df.voteshare.corr$date,
                                        format="%Y.%m.%d"
                                        )

df.voteshare.corr <- melt(df.voteshare.corr, id.vars=c("date", "N"))

plot.voteshare.corr <- ggplot(df.voteshare.corr,
                              aes(x=date,
                                  y=value,
                                  group=variable,
                                  colour=variable
                                  )
                              ) +
  geom_point(aes(size=N)) +
  geom_line() +
  scale_x_datetime("Prediction date") +
  scale_y_continuous("Correlation between predicted and actual Democratic voteshare")+
  scale_colour_discrete("Correspondence between predictions and outcomes")

ggsave(file="./figures/voteshare_winloss_correlation_bydate.pdf", plot=plot.voteshare.corr)


final.prediction <- house.votes[,c(1:2, ncol(house.votes))]
final.prediction$party.winner <-
  ifelse(final.prediction$vote_pct_display > 50, "Democrat", "Republican")

plot.final.prediction <- ggplot(final.prediction,
                                aes(x=vote_pct_display,
                                    y=X2012.11.06,
                                    label=state_dist
                                    )
                                ) +
  geom_smooth(method="lm", colour="black") +
  geom_text(aes(colour=party.winner), size=3) +
  scale_colour_manual("Election winner", values=c(Democrat="blue", Republican="red")) +
  scale_x_continuous("Actual Democratic vote share") +
  scale_y_continuous("Predicted Democratic vote share on 6 November 2012")+
  theme_bw()


ggsave(file="./figures/plot_corr_final_prediction_actual.png",
       plot=plot.final.prediction
       )


## Compare the predictions from 23 October (last good prediction) and
## 6 November. Clear pivot around a point at about 40% predicted vote
## share, this is where a lot of the bias creeps in.
final.prediction <- house.votes[,c(1:2, which(colnames(house.votes) %in%
                                              c("X2012.10.23",
                                                "X2012.11.06")
                                              )
                                   )
                                ]
final.prediction$party.winner <-
  ifelse(final.prediction$vote_pct_display > 50, "Democrat", "Republican")

fp.melt <- melt(final.prediction, id.vars=c("state_dist",
                                    "vote_pct_display", "party.winner"
                                    )
                )
levels(fp.melt$variable) <- c("23 October 2012",
                              "6 November 2012"
                              )



plot.datewise.predictions <- ggplot(fp.melt,
                                    aes(x=vote_pct_display,
                                        y=value,
                                        label=state_dist
                                        )
                                    ) +
  geom_smooth(method="lm", colour="black") +
  geom_text(aes(colour=party.winner), size=3) +
  geom_abline(intercept=0, slope=1, linetype=2) +
  facet_wrap(~ variable) +
  scale_colour_manual("Election winner", values=c(Democrat="blue", Republican="red")) +
  scale_x_continuous("Actual Democratic vote share") +
  scale_y_continuous("Predicted Democratic vote share")+
  theme_bw()


ggsave(file="./figures/plot_corr_23Oct_6Nov.png",
       plot=plot.datewise.predictions
       )
