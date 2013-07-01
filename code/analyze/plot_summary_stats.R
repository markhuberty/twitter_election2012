library(ggplot2)
library(reshape2)
library(scales)
setwd("~/projects/twitter_election2012")

convert.candidate.names <- function(statedistparty){

  state <- substr(statedistparty, 0, 2)
  dist <- substr(statedistparty, 3, 4)
  party <- substr(statedistparty, 6, 6)

  out <- cbind(state, dist, party)
  names(out) <- c("state", "district", "party")
  return(out)

}

## Load and format the per-candidate and per-district data
per.candidate.bydate.2010 <-
  read.csv("./data/summary_stats/tweets_per_candidate_per_day_2010.csv", header=FALSE)
per.candidate.bydate.2012 <-
  read.csv("./data/summary_stats/tweets_per_candidate_per_day_2012.csv", header=FALSE)

names(per.candidate.bydate.2010) <- names(per.candidate.bydate.2012) <-
  c("date", "unique_cand_id", "tweet_ct")

per.candidate.bydate.2010$year <- 2010
per.candidate.bydate.2012$year <- 2012


## Candidate volume timeseries, with a part-specific smoother
per.candidate.bydate <- rbind(per.candidate.bydate.2010,
                              per.candidate.bydate.2012
                              )
per.candidate.bydate$party <- substr(per.candidate.bydate$unique_cand_id, 6, 6)

per.candidate.bydate$date <- as.Date(per.candidate.bydate$date)

plot.candidate.volumes <- ggplot(per.candidate.bydate,
                                 aes(x=date,
                                     y=tweet_ct,
                                     group=unique_cand_id,
                                     linetype=party
                                     )
                                 ) +
  geom_point(size=1, alpha=0.1, position="jitter") +
  geom_smooth(aes(group=party), size=1, colour="black") +
  scale_y_log10("Log tweet count") +
  scale_x_date("Date") +
  facet_wrap(~ year, scales="free_x") +
  theme_bw()
print(plot.candidate.volumes)
ggsave(plot.candidate.volumes,
       file="./figures/plot_daily_candidate_volume.pdf",
       width=7,
       height=7
       )


## Distribution of total candidate message volume
per.candidate <- aggregate(per.candidate.bydate$tweet_ct,
                           by=list(per.candidate.bydate$unique_cand_id,
                             per.candidate.bydate$year),
                           sum
                           )
names(per.candidate) <- c("unique_cand_id", "year", "volume")
per.candidate$party <- substr(per.candidate$unique_cand_id, 6, 6)

plot.total.candidate.volumes <- ggplot(per.candidate,
                                       aes(x=volume,
                                           linetype=party
                                           )
                                       ) +
  geom_density() +
  scale_x_log10("Total tweet count per candidate") +
  scale_y_continuous("Density") +
  facet_wrap(~ year, scales="free_x") +
  scale_linetype("Party") +
  theme_bw()
print(plot.total.candidate.volumes)
ggsave(plot.total.candidate.volumes,
       file="./figures/plot_total_candidate_volume.pdf",
       width=7,
       height=7
       )

## Party volumes by day
per.party.byday <- aggregate(per.candidate.bydate$tweet_ct,
                             by=list(per.candidate.bydate$party,
                               per.candidate.bydate$date),
                             sum
                             )


names(per.party.byday) <- c("party", "date", "volume")
per.party.byday$date <- strptime(per.party.byday$date, format="%Y-%m-%d")
per.party.byday$year <- strftime(per.party.byday$date, format="%Y")

## Plot daily party volumnes
plot.daily.party.volume <- ggplot(per.party.byday,
                                  aes(x=as.Date(date),
                                      y=volume,
                                      colour=party
                                      )
                                  ) +
  geom_line() +
  scale_x_date("Date", breaks="1 week", labels=date_format("%A, %B %d, %Y")) +
  scale_y_continuous("Tweet Volume") +
  scale_colour_manual("Party", values=c("R" = "red", "D" = "blue")) +
  facet_wrap(~ year, scales="free_x") +
  theme_bw() +
  opts(axis.text.x=theme_text(angle=-90, hjust=0))

print(plot.daily.party.volume)
ggsave(plot.daily.party.volume,
       width=7,
       height=7,
       file="./figures/plot_daily_party_volume.pdf"
       )

## R vs. D volumes
per.candidate$district <- substr(per.candidate$unique_cand_id,
                                 1,
                                 4)
per.candidate.wide <- melt(per.candidate[,2:ncol(per.candidate)], id.vars=c("district", "year", "party"))

per.district <-
  aggregate(per.candidate.wide$value, by=list(per.candidate.wide$district), sum)
names(per.district) <- c("state_dist", "n.tweets")

candidates <- read.csv("./data/candidates_wide.csv")

candidates <- candidates[,c("state_dist", "incumbent_party", "D_name",
                            "R_name")
                         ]



candidates.melt <- melt(candidates, id.vars=c("state_dist",
                                      "incumbent_party")
                        )

candidates.melt$party <- substr(candidates.melt$variable, 1, 1)


per.candidate <- merge(per.candidate.wide,
                       candidates.melt,
                       by.x=c("district", "party"),
                       by.y=c("state_dist", "party"),
                       all=FALSE
                       )
names(per.candidate) <- c("state_dist", "party", "state", "district",
                          "cand.count", "incumbent_party", "variable",
                          "name"
                          )

per.candidate <- per.candidate[,c("state_dist", "incumbent_party",
                                  "party", "cand.count")
                               ]

per.candidate.melt <- melt(per.candidate,
                           id.vars=c("state_dist", "incumbent_party", "party")
                           )

## There are dups, not sure why...
per.candidate.melt <- unique(per.candidate.melt)

per.candidate.cast <- dcast(per.candidate.melt,
                            state_dist + incumbent_party ~
                            party + variable,
                            fun.aggregate=mean
                            )

## Generate the logged count data for both parties
per.candidate.cast$log.d.count <-
  log10(per.candidate.cast$D_cand.count + 1)
per.candidate.cast$log.r.count <-
  log10(per.candidate.cast$R_cand.count + 1)

## Plot the raw count data as a log/log plot
plot.cand.volumes <- ggplot(per.candidate.cast,
                            aes(x=D_cand.count + 1,
                                y=R_cand.count + 1,
                                colour=incumbent_party,
                                group=incumbent_party,
                                shape=incumbent_party)
                            ) +
  geom_abline(aes(intercept=0, slope=1), colour="black") +
  geom_point() +
  scale_x_log10("Log Democrat tweet volume") +
  scale_y_log10("Log Republican tweet volume") +
  scale_colour_manual("Incumbent Party", #values=c(D="blue", R="red",
                                        # O="green"),
                      labels=c("Democrat", "None", "Republican"),
                      values=c("black", "grey", "black")
                      ) +
  scale_shape_manual("Incumbent Party",
                     labels=c("Democrat", "None", "Republican"),
                     values=c(1, 2, 19)) +
  theme_bw()
print(plot.cand.volumes)
ggsave(plot.cand.volumes,
       height=7,
       width=7,
       file="./figures/plot_raw_cand_volumes.pdf"
       )

## Plot candidate volume density by party
plot.cand.volume.density <- ggplot(per.candidate.melt,
                                   aes(x=value,
                                       group=party,
                                       colour=party
                                       )
                                   ) +
  geom_density() +
  scale_x_continuous("Twitter volume by candidate", limits=c(0, 5000)) +
  scale_y_continuous("Density") +
  scale_colour_manual("Party", values=c("D" = "blue", "R" = "red"),
                      labels=c("Democrat", "Republican")
                      ) +
  theme_bw()
print(plot.cand.volume.density)
ggsave(plot.cand.volume.density,
       width=7,
       height=7,
       file='./figures/plot_cand_volume_density_byparty.pdf'
       )

## Plot the data by day

## Build a classifier to see how well we can predict
## the district incumbent from just the part-specific tweet volumes
library(e1071)
per.candidate.cast$inc_party_long <-
  per.candidate.cast$incumbent_party
levels(per.candidate.cast$inc_party_long) <- c("Democrat", "None", "Republican")
set.seed(23425)
sample.vec <- sample(1:nrow(per.candidate.cast),
                     floor(0.8*nrow(per.candidate.cast))
                     )
data.train <- per.candidate.cast[sample.vec, c("inc_party_long",
                                               "log.d.count",
                                               "log.r.count"
                                               )
                                 ]


data.test <- per.candidate.cast[-sample.vec, c("inc_party_long",
                                               "log.d.count",
                                               "log.r.count"
                                               )
                                 ]

## Tune the classifier across a reasonable set of ranges
classifier.on.volume <-
  tune(svm, inc_party_long ~ log.d.count + log.r.count,
       data=data.train,
       validation.y=data.test$inc_party_long,
       validation.x=data.test[,c("log.d.count", "log.r.count")],
       ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
       tunecontrol = tune.control(sampling = "cross")
       )

## Predict the outputs and check accuracy proportions by incumbent party
pred.vol <- predict(classifier.on.volume$best.model, newdata=data.test)
pred.tab <- table(data.test$inc_party_long, pred.vol)
pred.prop.tab <- prop.table(pred.tab,
                            margin=1
                            )




##' ggplot-based svm contour + points plot
##' @title ggplot.svm
##' @param svm.model an SVM model from the svm() routine in e1071
##' @param xyvars a data frame with the independent variables to make
##' up the X and Y dimensions of the contour plot. X should be in
##' column 1, y in column 2
##' @param class.vec the class data used in the SVM, corresponding to xyvars
##' @param xlab the label to use for the x axis
##' @param ylab the label to use for the y axis
##' @param fill.title the title for the fill legend
##' @param points.title the title for the points / class legend
##' @param plot.points boolean, should points be plotted?
##' @param save.plot boolean, should the figure be saved?
##' @param plot.filename valid file path and name for saving
##' @param print.plot boolean, should the figure be printed to the screen?
##' @param grid smoothing parameter for the SVM regions; higher is
##' smoother but takes much longer to plot
##' @param tile.alpha transparency for the contour layer
##' @param points.alpha transparency for the points layer
##' @param class.colors colors to be used in plotting the classes
##' @return silent return
##' @author Mark Huberty
ggplot.svm <-
  function(svm.model, xyvars,
           class.vec,
           xlab,
           ylab,
           fill.title,
           points.title,
           plot.points=TRUE,
           save.plot=TRUE,
           plot.filename=NULL,
           print.plot=TRUE,
           grid=200,
           tile.alpha=0.25,
           points.alpha=1,
           class.colors=NULL
           ){
    require(RColorBrewer)

    xdim <- xyvars[,1]
    ydim <- xyvars[,2]
    xr <- seq(min(xdim), max(xdim), length = grid)
    yr <- seq(min(ydim), max(ydim), length = grid)

    lis <- c(list(yr), list(xr))
    names(lis) <- rev(names(xyvars))
    new <- expand.grid(lis)[, labels(terms(svm.model))]
    preds <- predict(svm.model, new)
    df <- data.frame(new, preds)

    points.data <- data.frame(xyvars, class.vec)

    if(!plot.points)
      tile.alpha <- min(tile.alpha * 2, 1)

    if(is.null(class.colors))
      {

        class.colors <- brewer.pal(length(unique(class.vec)), "Dark2")

      }else{

        stopifnot(length(class.colors) == length(unique(class.vec)))

      }


    plot.contour <-
      ggplot(df,
             aes(x=df[,1],
                 y=df[,2]
                 )
             ) +
               geom_tile(aes(fill=preds), alpha=tile.alpha) +
                 scale_fill_manual(fill.title, breaks=levels(preds),
                                   values=class.colors
                                   ) +
                                     scale_x_continuous(xlab) +
                                       scale_y_continuous(ylab)


    if(plot.points)
      {

        plot.contour <- plot.contour + geom_point(df=points.data,
                                                  aes(x=points.data[,1],
                                                      y=points.data[,2],
                                                      col=class.vec
                                                      ),
                                                  alpha=points.alpha
                                                  ) +
                                                    scale_colour_manual(points.title,
                                                                        breaks=levels(class.vec),
                                                                        values=class.colors
                                                                        )

      }


    if(save.plot)
      ggsave(plot.contour, file=plot.filename)

    if(print.plot)
      print(plot.contour)

    return(plot.contour)

}

regions.plot <- ggplot.svm(classifier.on.volume$best.model,
           per.candidate.cast[,c("log.d.count", "log.r.count")],
           xlab="Democratic tweet volume (log scale)",
           ylab="Republican tweet volume (log scale)",
           class.vec=per.candidate.cast$inc_party_long,
           fill.title="Predicted incumbent party",
           points.title="Incumbent party",
           plot.points=FALSE,
           save.plot=TRUE,
           plot.filename="./figures/ggplot_svm_incumbency_regions.svg",
           print.plot=TRUE,
           class.colors=c("blue", "green", "red")
           )


regions.points.plot <- ggplot.svm(classifier.on.volume$best.model,
                                  per.candidate.cast[,c("log.d.count", "log.r.count")],
                                  xlab="Democratic tweet volume (log scale)",
                                  ylab="Republican tweet volume (log scale)",
                                  class.vec=per.candidate.cast$inc_party_long,
                                  fill.title="Predicted incumbent party",
                                  points.title="Incumbent party",
                                  plot.points=TRUE,
                                  save.plot=TRUE,
                                  plot.filename="./figures/ggplot_svm_incumbency_regions_points.svg",
                                  print.plot=TRUE,
                                  class.colors=c("blue", "green", "red")
                                  )

regions.plot <- ggplot.svm(classifier.on.volume$best.model,
           per.candidate.cast[,c("log.d.count", "log.r.count")],
           xlab="Democratic tweet volume (log scale)",
           ylab="Republican tweet volume (log scale)",
           class.vec=per.candidate.cast$inc_party_long,
           fill.title="Predicted incumbent party",
           points.title="Incumbent party",
           plot.points=FALSE,
           save.plot=TRUE,
           plot.filename="./figures/ggplot_svm_incumbency_regions.png",
           print.plot=TRUE,
           class.colors=c("blue", "green", "red")
           )


regions.points.plot <- ggplot.svm(classifier.on.volume$best.model,
                                  per.candidate.cast[,c("log.d.count", "log.r.count")],
                                  xlab="Democratic tweet volume (log scale)",
                                  ylab="Republican tweet volume (log scale)",
                                  class.vec=per.candidate.cast$inc_party_long,
                                  fill.title="Predicted incumbent party",
                                  points.title="Incumbent party",
                                  plot.points=TRUE,
                                  save.plot=TRUE,
                                  plot.filename="./figures/ggplot_svm_incumbency_regions_points.png",
                                  print.plot=TRUE,
                                  class.colors=c("blue", "green", "red")
                                  )
