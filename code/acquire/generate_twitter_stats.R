
# setwd(...)
source("./code/util/twitter.R")
load("./data/cron_output/master.cron.file.RData")
  
stats <- generateStats(tweets=master.cron.file)

for(i in 1:length(stats)){
  write.csv(stats[[i]], paste("./data/summary_stats/", names(stats)[i], ".csv", sep=""),
            row.names=TRUE)
}


# Now the JSON object:
per.district <- generateStats_JSON(tweets=master.cron.file)
write(per.district, file="./data/summary_stats/per_district.JSON")


