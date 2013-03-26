library(ggplot2)
library(reshape)
library(plyr)
sentiment.scores <-
  read.csv("./data/candidate_word_sentiments.csv")

## Aggregate sentiment by party and word

log.rescale <- function(vec){

  abs.vec <- abs(vec)
  log.abs.vec <- log(abs.vec + 1)
  vec.sign <- ifelse(vec < 0, -1, 1)
  sign.log.abs.vec <- log.abs.vec * vec.sign
  return(sign.log.abs.vec)

}

agg.fun <- function(x){

  log.out <- log.rescale(x$score)
  sd.log.out <- sd(log.out)
  out <- c(sum(x$score), mean(x$score), sd(x$score),
           sum(log.out), mean(log.out), sd.log.out)
  return(out)

}

party.sentiment <- ddply(sentiment.scores[,c("party", "stem",
                                             "score")],
                         .variables=c("party","stem"),
                         .fun="agg.fun"
                         )

names(party.sentiment) <- c("party", "word", "sentiment.score",
                            "mean.sentiment", "sd.sentiment",
                            "log.sentiment", "log.mean.sentiment",
                            "sd.log.mean.sentiment"
                            )

party.sentiment <- melt(party.sentiment,
                        id.vars=c("party", "word")
                        )
party.sentiment.cast <- cast(party.sentiment,
                             word ~ party + variable,
                             fill=0
                             )

party.sentiment.cast <-
  party.sentiment.cast[abs(party.sentiment.cast$D_mean.sentiment)
                       >= 1 |
                       abs(party.sentiment.cast$R_mean.sentiment)
                       >= 1,
                       ]

plot.party.sentiment <-
  ggplot(party.sentiment.cast[nchar(as.character(party.sentiment.cast$word)) > 2,],
         aes(x=D_log.mean.sentiment,
             y=R_log.mean.sentiment,
             ## ymin=R_mean.sentiment - R_sd.sentiment,
             ## ymax=R_mean.sentiment + R_sd.sentiment,
             label=word
             )
         ) +
  geom_text(hjust=1, vjust=1, size=3, alpha=0.5, position="jitter", colour="black") +
  geom_point(aes(size=R_sd.log.mean.sentiment), colour="red", alpha=0.25) +
  geom_point(aes(size=D_sd.log.mean.sentiment), colour="blue",
  alpha=0.25) +
  scale_x_continuous(limits=c(-2,2)) +
  scale_y_continuous(limits=c(-2,2)) +
  scale_size("Polarity score uncertainty")

ggsave(plot=plot.party.sentiment, file="./figures/word_sentiment.pdf",
       width=15, height=15)


plot.party.sentiment <-
  ggplot(party.sentiment.cast[nchar(as.character(party.sentiment.cast$word)) > 2,],
         aes(x=D_log.mean.sentiment,
             y=R_log.mean.sentiment,
             ## ymin=R_mean.sentiment - R_sd.sentiment,
             ## ymax=R_mean.sentiment + R_sd.sentiment,
             label=word
             )
         ) +
  geom_text(hjust=1, vjust=1, size=3, alpha=0.5, position="jitter", colour="black") +
  geom_point(aes(size=R_sd.log.mean.sentiment), colour="red", alpha=0.25) +
  geom_point(aes(size=D_sd.log.mean.sentiment), colour="blue", alpha=0.25) +
  scale_x_continuous(limits=c(0, 2)) +
  scale_y_continuous(limits=c(-2, 0))
ggsave(plot=plot.party.sentiment, file="./figures/word_sentiment_posD_negR.pdf")

plot.party.sentiment <-
  ggplot(party.sentiment.cast[nchar(as.character(party.sentiment.cast$word)) > 2,],
         aes(x=D_log.mean.sentiment,
             y=R_log.mean.sentiment,
             ## ymin=R_mean.sentiment - R_sd.sentiment,
             ## ymax=R_mean.sentiment + R_sd.sentiment,
             label=word
             )
         ) +
  geom_text(hjust=1, vjust=1, size=3, alpha=0.5, position="jitter", colour="black") +
  geom_point(aes(size=R_sd.log.mean.sentiment), colour="red", alpha=0.25) +
  geom_point(aes(size=D_sd.log.mean.sentiment), colour="blue", alpha=0.25) +
  scale_x_continuous(limits=c(0, 2)) +
  scale_y_continuous(limits=c(0, 2))
ggsave(plot=plot.party.sentiment, file="./figures/word_sentiment_posD_posR.pdf")

plot.party.sentiment <-
  ggplot(party.sentiment.cast[nchar(as.character(party.sentiment.cast$word)) > 2,],
         aes(x=D_log.mean.sentiment,
             y=R_log.mean.sentiment,
             ## ymin=R_mean.sentiment - R_sd.sentiment,
             ## ymax=R_mean.sentiment + R_sd.sentiment,
             label=word
             )
         ) +
  geom_text(hjust=1, vjust=1, size=3, alpha=0.5, position="jitter", colour="black") +
  geom_point(aes(size=R_sd.log.mean.sentiment), colour="red", alpha=0.25) +
  geom_point(aes(size=D_sd.log.mean.sentiment), colour="blue", alpha=0.25) +
  scale_y_continuous(limits=c(0, 2)) +
  scale_x_continuous(limits=c(-2, 0))
ggsave(plot=plot.party.sentiment, file="./figures/word_sentiment_negD_posR.pdf")

plot.party.sentiment <-
  ggplot(party.sentiment.cast[nchar(as.character(party.sentiment.cast$word)) > 2,],
         aes(x=D_log.mean.sentiment,
             y=R_log.mean.sentiment,
             ## ymin=R_mean.sentiment - R_sd.sentiment,
             ## ymax=R_mean.sentiment + R_sd.sentiment,
             label=word
             )
         ) +
  geom_text(hjust=1, vjust=1, size=3, alpha=0.5, position="jitter", colour="black") +
  geom_point(aes(size=R_sd.log.mean.sentiment), colour="red", alpha=0.25) +
  geom_point(aes(size=D_sd.log.mean.sentiment), colour="blue", alpha=0.25) +
  scale_x_continuous(limits=c(-2, 0)) +
  scale_y_continuous(limits=c(-2, 0))
ggsave(plot=plot.party.sentiment, file="./figures/word_sentiment_negD_negR.pdf")
