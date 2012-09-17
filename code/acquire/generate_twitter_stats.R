
# setwd(...)
source("./code/util/twitter.R")
load("./data/cron_output/master.cron.file.RData")
  
stats <- generateStats(tweets=master.cron.file, plot=FALSE)

for(i in 1:length(stats)){
  write.csv(stats[i], paste("./data/summary_stats/", names(stats)[i], ".csv", sep=""))
}


