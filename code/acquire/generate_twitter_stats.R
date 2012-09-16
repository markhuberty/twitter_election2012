
# setwd(...)
source("~/twitter_election2012/code/util/twitter.R")
master.cron.file <- load("~/twitter_election2012/data/cron_output/master.cron.file.RData")
  
stats <- generateStats(tweets=master.cron.file, plot=FALSE)

for(i in 1:length(stats)){
  write.csv(stats[i], paste(names(stats)[i], ".csv", sep=""))
}


