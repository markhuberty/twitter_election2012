## 17 November 2012
## Mark Huberty
## Inspection of the daily doc-term matrices suggests that the
## scripts were perhaps dropping a single row for each matrix after
## 10-23. This code systematically checks this.
setwd("/mnt/fwire_80/twitter_election2012")
library(tm)

daterange <- seq.Date(as.Date("2012-10-02"),
                      as.Date("2012-11-06"),
                      "day"
                      )

generic.tdm.masters <-
  paste("./data/doc_term_mat/generic.tdm.master.2.voteshare.",
        daterange,
        ".RData",
        sep=""
        )

daily.tdms <-
  paste("./data/doc_term_mat/generic.tdm.2.voteshare.",
        daterange,
        ".RData",
        sep=""
        )

idxrange <- 2:length(generic.tdm.masters)

for(idx in idxrange){


  load(generic.tdm.masters[idx])
  master.tdm <- tdm.corpus
  master.house <- house.data

  load(generic.tdm.masters[idx - 1])
  master.tdm.lag <- tdm.corpus
  master.house.lag <- house.data

  rm(house.data, tdm.corpus)
  gc()

  load(daily.tdms[idx])

  ## Check if the daily files are the same length
  test.daily.match <- nrow(house.data.temp) == dim(tdm.daily.corpus)[1]

  ## Check if the incremental composites are the same length
  new.master <- rbind(master.tdm.lag, tdm.daily.corpus)
  new.house <- rbind(master.house.lag, house.data.temp)
  test.new.match <- nrow(new.house) == dim(new.master)[1]

  ## Check if the first rows match; or if first row is moved down
  check.first.rows <- all(new.house[1,] == master.house[1,], na.rm=TRUE)
  check.row.shift <- all(new.house[2,] == master.house[1,], na.rm=TRUE)

  idx.row <- nrow(master.house.lag) + 1 + nrow(house.data.temp)
  check.nth.rows <- all(new.house[,] == master.house[,], na.rm=TRUE)
  ## Print diagnostics

  print(daterange[idx])
  print("Are daily files the same length?")
  print(test.daily.match)


  print("Are archive and reconstructed masters the same length?")
  print(test.new.match)


  if(check.first.rows)
    print("First rows match")
  else if(check.row.shift)
    print("First row of new master matches second row of old")

}
