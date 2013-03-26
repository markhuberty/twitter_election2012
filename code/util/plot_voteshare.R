library(ggplot2)
library(RColorBrewer)
setwd("~/Documents/Research/Papers/twitter_election2012/")
voteshare <- read.csv("./data/continuous.prediction.master.csv")
winloss <- read.csv("./data/binary.prediction.master.csv")
voteshare$prediction.date <- as.Date(as.character(voteshare$prediction.date))
winloss$prediction.date <- as.Date(as.character(winloss$prediction.date))
incumbents <- read.csv("./data/incumbency.clean.csv")

districts <-
  read.csv("./data/districts.csv",
           stringsAsFactors=FALSE
           )


district.incumb <- merge(districts,
                         incumbents,
                         by.x=c("state", "district"),
                         by.y=c("district.state", "district.num"),
                         all.x=TRUE,
                         all.y=FALSE
                         )
voteshare.split <- split(voteshare, voteshare$prediction.date)
voteshare.test <- lapply(voteshare.split, function(x){

  temp <- merge(x,
                district.incumb,
                by.x="state_district",
                by.y="state_dist",
                all.x=TRUE,
                all.y=TRUE
                )
  temp$party.win <- ifelse(temp$dem_vote_share > 50,
                           "D",
                           "R"
                           )
  temp$all.win <- ifelse(is.na(temp$party.win),
                         substr(as.character(temp$incumbents.Party),0,1),
                         as.character(temp$party.win)
                         )

  return(temp)
})


voteshare <- merge(voteshare,
                   district.incumb,
                   by.x="state_district",
                   by.y="state_dist",
                   all=FALSE
                   )
winloss <- merge(winloss,
                 district.incumb,
                 by.x="state_district",
                 by.y="state_dist",
                 all=FALSE
                 )

voteshare$incumbent_party <- as.character(voteshare$incumbent_party)
winloss$incumbent_party <- as.character(winloss$incumbent_party)
voteshare$incumbent_party[voteshare$incumbent_party==""] <-
  "ND"
winloss$incumbent_party[winloss$incumbent_party==""] <-
  "ND"

voteshare$rating <- as.factor(voteshare$rating)
winloss$rating <- as.factor(winloss$rating)
voteshare$rating <- factor(voteshare$rating,
                           levels=levels(voteshare$rating)[rev(c(3,1,5,2,4))]
                           )
winloss$rating <- factor(winloss$rating,
                         levels=levels(winloss$rating)[rev(c(3,1,5,2,4))]
                         )

plot.voteshare.rating <- ggplot(voteshare,
                                aes(x=prediction.date,
                                    y=dem_vote_share,
                                    group=state_district,
                                    colour=rating
                                    )
                                ) +
  geom_line(alpha=0.4) +
  scale_colour_brewer("NYT Partisan Rating", palette="RdBu") +
  scale_x_date("Prediction date") +
  scale_y_continuous("Predicted Democratic vote share")
pdf("./figures/pred_voteshare_rating.pdf")
print(plot.voteshare.rating)
dev.off()

plot.voteshare.inc <- ggplot(voteshare,
                                aes(x=prediction.date,
                                    y=dem_vote_share,
                                    group=state_district,
                                    colour=incumbent_party
                                    )
                                ) +
  geom_line(alpha=0.7) +
  scale_colour_manual("Incumbent party",
                      values=c("R"="red",
                        "D"="blue",
                        "ND"="green",
                        "none"="grey"
                        )
                      ) +
  scale_x_date("Prediction date") +
  scale_y_continuous("Predicted Democratic vote share")
print("./figures/pred_voteshare_incumb.pdf")
print(plot.voteshare.inc)
dev.off()

plot.winloss.rating <- ggplot(winloss,
                              aes(x=prediction.date,
                                  y=prob.d.win,
                                  group=state_district,
                                  colour=rating
                                  )
                              ) +
  geom_line(alpha=0.7) +
  scale_colour_brewer("NYT Partisan Rating", palette="RdBu") +
  scale_x_date("Prediction date") +
  scale_y_continuous("Probability of Democratic win")
pdf("./figures/pred_winloss_rating.pdf")
print(plot.winloss.rating)
dev.off()

plot.winloss.inc <- ggplot(winloss,
                           aes(x=prediction.date,
                               y=prob.d.win,
                               group=state_district,
                               colour=incumbent_party
                               )
                           ) +
  geom_line(alpha=0.7) +
    scale_colour_manual("Incubment Party",
                      values=c("R"="red",
                        "D"="blue",
                        "ND"="green",
                        "none"="grey"
                        )
                      ) +
  scale_x_date("Prediction date") +
  scale_y_continuous("Probability of Democratic win")
pdf("./figures/pred_winloss_incumb.pdf")
print(plot.winloss.inc)
dev.off()


all.predictions <- merge(voteshare,
                         winloss,
                         by=c("state_district",
                           "prediction.date"
                           ),
                         all=FALSE
                         )

plot.pred.corr.incumb <- ggplot(all.predictions,
                                aes(x=dem_vote_share,
                                    y=prob.d.win,
                                    colour=incumbent_party.x
                                    )
                                ) +
  geom_point() +
  scale_colour_manual("Incubment Party",
                      values=c("R"="red",
                        "D"="blue",
                        "ND"="green",
                        "none"="grey"
                        )
                      ) +
  scale_x_continuous("Predicted Democratic vote share") +
  scale_y_continuous("Predicted Democratic win probability")
pdf("./figures/pred_correlation_incumb.pdf")
print(plot.pred.corr.incumb)
dev.off()

scale.date <- function(d){
  di <- as.integer(d)
  di <- di - min(di) + 1
  return(di)
}

plot.pred.corr.rating <- ggplot(all.predictions,
                               aes(x=dem_vote_share,
                                   y=prob.d.win,
                                   colour=rating.x,
                                   alpha=scale.date(prediction.date)
                                   )
                                ) +
  geom_point() +
  scale_colour_brewer("NYT Partisan Rating", palette="RdBu") +
  scale_x_continuous("Predicted Democratic vote share") +
  scale_y_continuous("Predicted Democratic win probability")
pdf("./figures/pred_correllation_rating.pdf")
print(plot.pred.corr.rating)
dev.off()

test <- ggplot(winloss,
               aes(x=prediction.date,
                   y=prob.d.win,
                   group=prediction.date
                   )
               ) +
  geom_boxplot() +
  facet_wrap(~ incumbent_party)
print(test)
