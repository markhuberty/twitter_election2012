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
