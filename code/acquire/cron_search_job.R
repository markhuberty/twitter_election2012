source("./code/util/twitter.R")

require(doMC)
require(multicore)
require(foreach)
require(rjson)
require(RCurl)

debug <- FALSE
## The master data. Only first.time is here
load("./data/cron.input.data.RData")

## If not the first time, load up the aggregate file
if(first.time != 1)
{
  load("./data/cron_output/master.cron.file.RData")
}else{
  print("First loop")
}

## Read in the candidates we will query
candidates <- read.csv("./data/candidates.final.2012.csv")

## Identify those who are going to be queried.
## This shouldn't be an issue in the final candidate set
## But is only here for safekeeping
## candidates.pull <- candidates[!is.na(candidates$last.name) &
##3                              candidates$last.name != "",]

candidates.pull <-
  candidates[candidates$unique_cand_id != "none",]

if(debug)
  candidates.pull <- candidates.pull[1:50,]

## Write the encoded query string.
## %22 here is the code for quotes to make sure
## that the whole name is mentioned.
pull.list <- c()
for(i in 1:nrow(candidates.pull)) {

  pull.list <- c(pull.list,
                 paste("%22",
                       gsub(" ", "+", candidates.pull$first_name[i]),
                       "+",
                       gsub(" ", "+", candidates.pull$last_name[i]),
                       "%22",
                       sep=""
                       )
                 )
}

if(first.time==1)
{
  file.today <- search.twitter.pages(terms=pull.list,
                                     rpp=100,
                                     delay.interval=1,
                                     since.date=Sys.Date()-7,
                                     write.out=FALSE
                                     )

}else{
  ## If the job has run before, then only get updates
  ## from yesterday. (Assumes the job runs nightly)
  yesterday <- Sys.Date() - 1
  file.today <- search.twitter.pages(terms=pull.list,
                                     rpp=100,
                                     since.date=yesterday,
                                     delay.interval=1,
                                     write.out=FALSE
  )

}

names(file.today) <- pull.list

results.fields.desired <- c("profile_image_url",
                            "created_at",
                            "from_user",
                            "metadata.result_type",
                            "to_user_id",
                            "text",
                            "id",
                            "from_user_id",
                            "to_user",
                            "iso_language_code",
                            "source"
                            )

cron.filename <- paste("./data/cron_output/cron.json.list.",
                       Sys.Date(),
                       ".RData",
                       sep=""
                       )
save(file.today,
     file=cron.filename
     )
## Parse the file immediately
file.today.parsed <-
  parse.json.out(file.today,
                 results.fields.desired,
                 unique.cand.id=as.character(candidates.pull$unique_cand_id),
                 num.cores=2
                 )

## Select for only English words
## Replace URL encoding for quotes and ampersands with the real thing
file.today.parsed.en <-
  file.today.parsed[file.today.parsed$iso_language_code=="en"&
                    !is.na(file.today.parsed$text),
                    ]


file.today.parsed.en$text <- gsub("&quot;", "", file.today.parsed.en$text)
file.today.parsed.en$text <- gsub("&amp;", "&", file.today.parsed.en$text)

## Drop any dups that we get by virtue of the moving window problem
file.today.parsed.en <- unique(file.today.parsed.en)

## Save the time/date stamped cron file
save(file.today.parsed.en,
     file=paste("./data/cron_output/cron.file.",
       gsub(" ", "", Sys.Date()), ".RData",
       sep=""
       )
     )

## Write to the master file
if(first.time==1)
{

  master.cron.file <- file.today.parsed.en

}else{

  master.cron.file <- rbind(master.cron.file,
                            file.today.parsed.en
                            )
}
#since.id <- max(file.today.parsed.en$id)

#save(since.id, "cron.since.id.RData")


## Save the output and log the job completion.
first.time <- 0
save(master.cron.file, file="./data/cron_output/master.cron.file.RData")
save(first.time, file="./data/cron.input.data.RData")

sink(file="./logs/cron.job.log", append=TRUE, type="output")
print(paste("Cron job for ", Sys.Date(), "completed"))
print(paste("Size of today's matrix:", dim(file.today.parsed.en)))
print(paste("Size of master file:", dim(master.cron.file)))
sink()
