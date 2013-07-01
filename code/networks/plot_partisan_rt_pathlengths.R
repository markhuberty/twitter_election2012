library(ggplot2)
library(plyr)
rt2010 <- read.csv("../../data/rt_path_lengths_2010.csv")
rt2012 <- read.csv("../../data/rt_path_lengths_2012.csv")
rt2010$year <- 2010
rt2012$year <- 2012
rt.pathlengths <- rbind(rt2010, rt2012)

mt2010 <- read.csv("../../data/mt_path_lengths_2010.csv")
mt2012 <- read.csv("../../data/mt_path_lengths_2012.csv")
mt2010$year <- 2010
mt2012$year <- 2012
mt.pathlengths <- rbind(mt2010, mt2012)

## Bootstrap the distribution around the mean
bs.val <- function(x, probs=c(0.025, 0.975), n){

  bs.mean <- sapply(1:n, function(y){

    x.sample <- x[sample(1:length(x), replace=TRUE)]
    mean.x <- mean(x.sample)
    return(mean.x)
  })

  mean.x <- mean(bs.mean)
  quantile.x <- quantile(bs.mean, probs=probs)
  return(c(mean.x, quantile.x))

}

bs.wrapper <- function(df){

  out <- bs.val(df$length, n=1000)
  return(out)

}

## Do the RT analysis
rt.pathlength.cis <- ddply(.data=rt.pathlengths,
                           .variables=c("year", "class"),
                           .fun=bs.wrapper
                           )
rt.pathlength.cis$type <- "Retweet"

## Do the MT analysis
mt.pathlength.cis <- ddply(.data=mt.pathlengths,
                           .variables=c("year", "class"),
                           .fun=bs.wrapper
                           )
mt.pathlength.cis$type <- "Mention"

pathlength.cis <- rbind(rt.pathlength.cis,
                        mt.pathlength.cis
                        )
levels(pathlength.cis$class) <- c("Conservative", "Liberal", "Cons.-Lib.")
names(pathlength.cis) <- c("year", "class", "path.length", "ci2.5", "ci97.5", "type")

plot.pathlengths <- ggplot(pathlength.cis,
                           aes(x=class,
                               y=path.length,
                               ymin=ci2.5,
                               ymax=ci97.5,
                               colour=as.factor(year)
                               )
                           ) +
  #geom_point(size=1) +
  geom_errorbar(width=0.2) +
  scale_y_continuous("Path length", limits=c(0, 8)) +
  scale_x_discrete("Partisan link") +
  scale_colour_grey("Year", start=0, end=0.5) +
  coord_flip() +
  facet_wrap(~ type) +
  theme_bw()
print(plot.pathlengths)
ggsave(plot.pathlengths,
       file="../../figures/plot_rt_mt_pathlengths_2010_2012.pdf",
       width=6,
       height=2
       )

t.lib.con <- t.test(rt.pathlengths$length[rt.pathlengths$class=="lib"],
                    rt.pathlengths$length[rt.pathlengths$class=="con"]
                    )
t.lib.lib_con <- t.test(rt.pathlengths$length[rt.pathlengths$class=="lib"],
                        rt.pathlengths$length[rt.pathlengths$class=="lib_con"]
                        )
t.con.lib_con <- t.test(rt.pathlengths$length[rt.pathlengths$class=="con"],
                        rt.pathlengths$length[rt.pathlengths$class=="lib_con"]
                        )

notate.labels <- c("Mean", "2.5% CI", "97.5%CI")
plot.path.density <- ggplot(rt.pathlengths,
                            aes(x=length,
                                group=class,
                                linetype=class
                                )
                            ) +
  geom_density() +
  scale_linetype("Path type",
                 #values=c("lib" = "blue", "con" = "red", "lib_con" = "purple"),
                 labels=c("Cons.-Cons.", "Lib.-Lib.", "Lib.-Cons.")
                 ) +
  scale_x_continuous("Path length") +
  scale_y_continuous("Density") +
  facet_wrap(~ year) +
  theme_bw()
print(plot.path.density)
ggsave(plot.path.density,
       width=7,
       height=7,
       file="../../figures/rt_graph_path_length_distribution.pdf"
       )


notate.labels <- c("Mean", "2.5% CI", "97.5%CI")
plot.path.density <- ggplot(mt.pathlengths,
                            aes(x=length,
                                group=class,
                                linetype=class
                                )
                            ) +
  geom_density() +
  scale_linetype("Path type",
                 #values=c("lib" = "blue", "con" = "red", "lib_con" = "purple"),
                 labels=c("Cons.-Cons.", "Lib.-Lib.", "Lib.-Cons.")
                 ) +
  scale_x_continuous("Path length") +
  scale_y_continuous("Density") +
  facet_wrap(~ year) +
  theme_bw()
print(plot.path.density)
ggsave(plot.path.density,
       width=7,
       height=7,
       file="../../figures/mt_graph_path_length_distribution.pdf"
       )

mt.pathlengths$type <- "Mention"
rt.pathlengths$type <- "Retweet"
all.pathlengths <- rbind(mt.pathlengths,
                         rt.pathlengths
                         )


plot.path.density <- ggplot(all.pathlengths,
                            aes(x=length,
                                group=class,
                                linetype=class
                                )
                            ) +
  geom_density() +
  scale_linetype("Path type",
                 #values=c("lib" = "blue", "con" = "red", "lib_con" = "purple"),
                 labels=c("Cons.-Cons.", "Lib.-Lib.", "Lib.-Cons.")
                 ) +
  scale_x_continuous("Path length") +
  scale_y_continuous("Density") +
  facet_grid(type ~ year) +
  theme_bw()
print(plot.path.density)
ggsave(plot.path.density,
       width=7,
       height=7,
       file="../../figures/mt_rt_graph_path_length_distribution.pdf"
       )
