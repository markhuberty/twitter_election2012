library(ggplot2)
rt.pathlengths <- read.csv("../../data/gnb_rt_path_lengths.csv")
mt.pathlengths <- read.csv("../../data/gnb_mt_path_lengths.csv")
## Bootstrap the distribution around the mean
bs.val <- function(x, probs=c(0.025, 0.975), n){

  bs.mean <- sapply(1:n, function(y){

    x.sample <- x[sample(1:length(y), replace=TRUE)]
    mean.x <- mean(x.sample)
    return(mean.x)
  })

  mean.x <- mean(bs.mean)
  quantile.x <- quantile(bs.mean, probs=probs)
  return(c(mean.x, quantile.x))

}

## Do the RT analysis
lib.ci <- bs.val(rt.pathlengths$length[rt.pathlengths$class=="lib"], n=1000)
con.ci <- bs.val(rt.pathlengths$length[rt.pathlengths$class=="con"], n=1000)
lib.con.ci <- bs.val(rt.pathlengths$length[rt.pathlengths$class=="lib_con"],
                     n=1000
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
                                colour=class
                                )
                            ) +
  geom_density() +
  scale_colour_manual("Path type",
                      values=c("lib" = "blue", "con" = "red", "lib_con" = "purple"),
                      labels=c("Cons.-Cons.", "Lib.-Lib.", "Lib.-Cons.")
                      ) +
  scale_x_continuous("Path length") +
  scale_y_continuous("Density") +
  theme_bw()
print(plot.path.density)
ggsave(plot.path.density,
       width=7,
       height=7,
       file="../../figures/rt_graph_path_length_distribution.pdf")

## Do the MT analysis
lib.ci <- bs.val(mt.pathlengths$length[mt.pathlengths$class=="lib"], n=1000)
con.ci <- bs.val(mt.pathlengths$length[mt.pathlengths$class=="con"], n=1000)
lib.con.ci <- bs.val(mt.pathlengths$length[mt.pathlengths$class=="lib_con"],
                     n=1000
                     )

t.lib.con <- t.test(mt.pathlengths$length[mt.pathlengths$class=="lib"],
                    mt.pathlengths$length[mt.pathlengths$class=="con"]
                    )
t.lib.lib_con <- t.test(mt.pathlengths$length[mt.pathlengths$class=="lib"],
                        mt.pathlengths$length[mt.pathlengths$class=="lib_con"]
                        )
t.con.lib_con <- t.test(mt.pathlengths$length[mt.pathlengths$class=="con"],
                        mt.pathlengths$length[mt.pathlengths$class=="lib_con"]
                        )

notate.labels <- c("Mean", "2.5% CI", "97.5%CI")
plot.path.density <- ggplot(mt.pathlengths,
                            aes(x=length,
                                group=class,
                                colour=class
                                )
                            ) +
  geom_density() +
  scale_colour_manual("Path type",
                      values=c("lib" = "blue", "con" = "red", "lib_con" = "purple"),
                      labels=c("Cons.-Cons.", "Lib.-Lib.", "Lib.-Cons.")
                      ) +
  scale_x_continuous("Path length") +
  scale_y_continuous("Density") +
  theme_bw()
print(plot.path.density)
ggsave(plot.path.density,
       width=7,
       height=7,
       file="../../figures/mt_graph_path_length_distribution.pdf")
