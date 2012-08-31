## Simple script to reset the "first.time" object between tests
setwd("../../data/")
first.time <- 1
save.image("cron.input.data.RData")
quit()
