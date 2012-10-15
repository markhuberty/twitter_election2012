library(ggplot2)
library(Hmisc)
library(foreach)
library(reshape)

## Load the master candidate file w/ all districts
cand.vis <- read.csv("./data/candidates.final.vis.2012.csv")

## Load our candidate file
cand.orig <- read.csv("./data/candidates.final.2012.csv")

## Load the voteshare data
voteshare <- read.csv("./data/continuous.prediction.master.wide.csv")

## Grab the latest voteshare:

latest.prediction <- voteshare[,c(1, ncol(voteshare))]
names(latest.prediction) <- c("state_dist", "voteshare")

latest.prediction <- merge(latest.prediction,
                           cand.vis,
                           by="state_dist",
                           all=TRUE
                           )

latest.prediction$winloss <- ifelse(latest.prediction$voteshare > 50,
                                    "D",
                                    "R"
                                    )
latest.prediction$winloss[is.na(latest.prediction$winloss)] <-
  as.character(latest.prediction$incumbent_party[is.na(latest.prediction$winloss)])

d.r.today <- table(latest.prediction$winloss)
print(d.r.today)
print(prop.table(d.r.today))

## Compute the prediction by day since the start

all.predictions <- merge(voteshare,
                         cand.vis[,c("state_dist",
                         "incumbent_party")],
                         by.x="state_district",
                         by.y="state_dist",
                         all=TRUE
                         )

compute.winloss <- function(predictions, threshold=50){
  pred.by.day <- foreach(i=2:(ncol(predictions) - 1), .combine=cbind) %do% {

    pred <- ifelse(predictions[,i] >=threshold,
                   "D",
                   "R"
                   )
    pred[is.na(pred)] <- as.character(predictions$incumbent_party[is.na(pred)])
    return(pred)



  }
  colnames(pred.by.day) <-
    colnames(predictions)[2:(ncol(predictions) - 1)]
  return(as.data.frame(pred.by.day))
}

daily.makeup <- compute.winloss(all.predictions)
daily.makeup$state_dist <- as.character(all.predictions$state_dist)

daily.makeup.melt <- melt(daily.makeup, id.vars="state_dist")
names(daily.makeup.melt) <- c("state_dist", "date", "winning.party")
daily.makeup.melt$date <- as.Date(gsub("X", "", daily.makeup.melt$date),
                                  format="%Y.%m.%d"
                                  )

## Aggregate counts by party and date

daily.counts <- ddply(daily.makeup.melt, .variables=c("date", "winning.party"),
                      .fun="nrow"
                      )
levels(daily.counts$winning.party) <- c("Democrat",
                                        "Open",
                                        "Republican"
                                        )

plot.daily.seatcounts <- ggplot(daily.counts,
                                aes(x=date,
                                    y=nrow,
                                    colour=winning.party,
                                    group=winning.party
                                    )
                                ) +
  geom_line(size=2) +
  scale_x_date("Prediction date") +
  scale_y_continuous("Predicted party seatcount in Congress") +
  scale_colour_manual("Party", values=c(Republican="red", Democrat="blue", Open="green"))
ggsave("./figures/predicted_party_seatcount_bydate.png")
