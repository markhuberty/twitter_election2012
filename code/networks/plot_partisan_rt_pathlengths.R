library(ggplot2)
df <- read.csv("../../data/rt_path_lengths.csv")
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

lib.ci <- bs.val(df$length[df$class=="lib"], n=1000)
con.ci <- bs.val(df$length[df$class=="con"], n=1000)
lib.con.ci <- bs.val(df$length[df$class=="lib_con"],
                     n=1000
                     )

t.lib.con <- t.test(df$length[df$class=="lib"],
                    df$length[df$class=="con"]
                    )
t.lib.lib_con <- t.test(df$length[df$class=="lib"],
                        df$length[df$class=="lib_con"]
                        )
t.con.lib_con <- t.test(df$length[df$class=="con"],
                        df$length[df$class=="lib_con"]
                        )

notate.labels <- c("Mean", "2.5% CI", "97.5%CI")
plot.path.density <- ggplot(df,
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
  theme_bw() +
  coord_cartesian(xlim=c(0, 25))##  +
  ## geom_text(aes(20,
  ##               0.3,
  ##               label=paste("Lib:",
  ##                 paste(notate.labels, lib.ci, collapse=", ")
  ##                 )
  ##               ),
  ##           hjust=0,
  ##           col="black"
  ##           ) +
  ## geom_text(aes(20,
  ##               0.25,
  ##               label=paste("Con:",
  ##                 paste(notate.labels,
  ##                       con.ci,
  ##                       collapse=", ")
  ##                 )
  ##               ),
  ##           hjust=0,
  ##           col="black"
  ##           ) +
  ## geom_text(aes(20,
  ##               0.2,
  ##               label=paste("Lib-Con:",
  ##                 paste(notate.labels,
  ##                       lib.con.ci,
  ##                       collapse=", ")
  ##                 )
  ##               ),
  ##           hjust=0,
  ##           col="black"
  ##          )

print(plot.path.density)
ggsave(plot.path.density, file="../../figures/rt_graph_path_length_distribution.pdf")
