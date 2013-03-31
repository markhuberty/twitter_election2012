library(rjson)
library(RCurl)
library(foreach)

nyt.house.results <-
  getURL("http://elections.nytimes.com/2012/results/house/big-board.json")

nyt.list <- fromJSON(nyt.house.results)

parse.json.results <- function(result.list){

  ## Here's a two-liner to parse the race data (!)
  ## See here for more detail:
  ## http://stackoverflow.com/questions/3409583/converting-uneven-hierarchical-list-to-a-data-frame
  race.results <- lapply(result.list$races, function(x) x$results)
  results.data.frame <-
    ldply(llply(race.results,
                function(x){ldply(x,
                                  function(y) rbind.fill(data.frame(y)
                                                         )
                                  )
                          }
                )
          )

  race.attributes <- foreach(r=result.list$races, .combine=rbind) %do% {

    this.result <- r$results
    state <- r$state_id
    uncontested <- r$uncontested
    seat <- r$seat_number
    incumbent <- r$incumbent_party

    attr <- c(state, uncontested, seat, incumbent)

    attr.df <- foreach(1:length(this.result), .combine=rbind) %do% {

      attr

    }

    return(attr.df)
  }
  race.attributes <- as.data.frame(race.attributes)
  colnames(race.attributes) <- c("state", "uncontested", "seat", "incumbent_party")
  out <- cbind(race.attributes, results.data.frame)
  return(out)

}

house.results <- parse.json.results(nyt.list)
house.results$state_dist <-
  ifelse(nchar(as.character(house.results$seat)) == 1,
         paste(house.results$state, "0", house.results$seat, sep=""),
         paste(house.results$state, house.results$seat, sep="")
         )

write.csv(house.results, file="./data/house_vote_results.csv")

## ## Some tests
## library(reshape)
## predictions <- read.csv("./data/continuous.prediction.master.csv")
## predictions <- melt(predictions)
## predictions$state_district <-
##   gsub("00", "01", predictions$state_district)

## predictions.wide <- cast(predictions, state_district ~
##                          prediction.date)

## compute.win.rate <- function(vote){

##   tab <- table(vote >= 50)
##   rate <- tab / sum(tab)
##   return(rate)
## }

## house.results <- house.results[house.results$state_dist %in%
##                                predictions.wide$state_district, ]
## house.results$winner <-
##   as.numeric(as.character(house.results$vote_pct_display)) >= 50

## rep.incumbents <-
##   house.results[house.results$incumbent_party == "rep" &
##                 house.results$incumbent==TRUE &
##                 house.results$party_id == "REP", ]

## dem.incumbents <-
##   house.results[house.results$incumbent_party == "dem" &
##                 house.results$incumbent==TRUE &
##                 house.results$party_id == "DEM", ]

## twoparty.incumbents <-
##   house.results[house.results$incumbent_party %in% c("rep", "dem") &
##                 house.results$incumbent==TRUE &
##                 house.results$party_id %in% c("REP", "DEM"), ]

## rep.inc.winrate <-
##   compute.win.rate(as.numeric(as.character(rep.incumbents$vote_pct_display)))
## dem.inc.winrate <-
##   compute.win.rate(as.numeric(as.character(dem.incumbents$vote_pct_display)))
## twoparty.inc.winrate <-
##   compute.win.rate(as.numeric(as.character(twoparty.incumbents$vote_pct_display)))

## (mean(d$incumbent == d$winner)*(sum(d$incumbent)) +
##   mean(r$incumbent == r$winner)*(sum(r$incumbent))) / sum(r$incumbent +
## d$incumbent)
