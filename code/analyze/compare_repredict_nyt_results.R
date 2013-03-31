library(rjson)
library(RCurl)
library(foreach)
library(ggplot2)
library(reshape)

setwd("~/Documents/Research/Papers/twitter_election2012/")
house.results <- read.csv("./data/house_vote_results.csv")
## Load up the prediction data
#voteshare <- read.csv("~/continuous.prediction.master.wide.csv")

voteshare <- read.csv("./predictions/continuous.prediction.master.csv")

voteshare <- melt(voteshare)
voteshare <- cast(voteshare, state_district ~ prediction.date)

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
                                        format="%Y-%m-%d"
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

winning.party <- ifelse(house.votes$vote_pct_display > 50,
                        "Democrat",
                        "Republican"
                        )

plot.voteshare.district <- ggplot(house.votes,
                                  aes(x=vote_pct_display,
                                      y=house.votes[,"2012-11-06"],
                                      label=state_dist,
                                      colour=winning.party
                                      )
                                  ) +
  geom_vline(xintercept=50, alpha=0.5, colour="green") +
  geom_hline(yintercept=50, alpha=0.5, colour="green") +
  geom_text() +
  scale_colour_manual("Electoral winner",
                      values=c("Republican" = "red", "Democrat" = "blue")) +
  scale_x_continuous("Actual Democratic voteshare") +
  scale_y_continuous("Predicted Democratic voteshare, 11-06-2012") +
  theme_bw() +
  opts(axis.title.x=theme_text(size=20),
       axis.title.y=theme_text(size=20, angle=90)
       )

ggsave(file="./figures/plot_repredict_actual_corr.pdf",
       plot=plot.voteshare.district
       )

print(plot.voteshare.district)


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
                                        format="%Y-%m-%d"
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
  scale_y_continuous("")+
  scale_colour_discrete("Correspondence between predictions and outcomes") +
  theme_bw()

ggsave(file="./figures/repredict_voteshare_winloss_correlation_bydate.pdf", plot=plot.voteshare.corr)



## Do some analysis of what votes changed the most

vote.diff.oct.nov <- house.votes[,"2012-11-06"] - house.votes[,"2012-10-22"]
switch.victor <-
  house.votes[,"2012-11-06"] < 50 & house.votes[,"2012-10-22"] > 50

df.vote.diff <- data.frame(house.votes$state_dist, vote.diff.oct.nov, switch.victor)

districts.to.inspect <-
  df.vote.diff[order(df.vote.diff$vote.diff.oct.nov),][1:20,]

## Load up
dtm.to.sparse <- function(tdm){
  sparse.corpus <- sparseMatrix(i=tdm$i,
                                j=tdm$j,
                                x=tdm$v,
                                dims=c(tdm$nrow,
                                  tdm$ncol)
                                )
  return(sparse.corpus)
}

load("~/generic.tdm.master.2.voteshare.2012-10-23.RData")
oct.corpus <- dtm.to.sparse(tdm.corpus)

load("~/generic.tdm.master.2.voteshare.2012-11-06.RData")
nov.corpus <- dtm.to.sparse(tdm.corpus)

col.names <- unlist(tdm.corpus$dimnames[2])

oct.term.sums <- colSums(oct.corpus)
nov.term.sums <- colSums(nov.corpus)

oct.term.shares <- oct.term.sums / sum(oct.term.sums)
nov.term.shares <- nov.term.sums / sum(nov.term.sums)

df <- data.frame(col.names,
                 oct.term.sums,
                 nov.term.sums,
                 oct.term.shares,
                 nov.term.shares
                 )

plot.termshares <- ggplot(df,
                          aes(x=oct.term.shares,
                              y=nov.term.shares,
                              label=col.names,
                              size=oct.term.shares
                              )
                          ) +
  geom_text()

print(plot.termshares)

df$sharediff <- df$nov.term.shares - df$oct.term.shares

df <- df[order(df$sharediff, decreasing=TRUE),]
df$col.names <- factor(df$col.names,
                       levels=df$col.names[order(df$sharediff,
                         decreasing=TRUE)]
                       )

plot.col.diffs <- ggplot(df,
                         aes(x=col.names, y=sharediff, label=col.names)
                         ) +
  geom_text(size=2, alpha=0.5, position="jitter", hjust=0)
print(plot.col.diffs)
