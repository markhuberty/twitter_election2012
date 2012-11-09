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
