## Infrastructure code to grab the twitter stream
## 2012 Congressional Election
## Mark Huberty
## Begun 27 May 2010

# library(RCurl) ## to submit the http requests
# library(rjson) ## to parse the JSON results
# library(methods)
# library(foreach) ## to parallelize the post-processing for speed
# library(doMC)    ## to parallelize the post-processing for speed
# library(twitteR)

## This file will provide the scripting functions necessary to query the Twitter
## Search API (http://apiwiki.twitter.com/Twitter-Search-API-Method:-search)
## Limitations:
## Twitter SEARCH API is not rate-limited by any defined limit.
## It appears that 1500 requests/day would work

## Goal:
## These functions will run starting at the opening of polls on Election Day 2010 and run until the close of polls.
## Want to grab tweets, users, retweets
## Restrict language to English
## Restrict by locale if possible
## Do for all 50 states
##
## URL:
## http://search.twitter.com/search.format

## Formats:
## json, atom

## HTTP Method:
## GET

## Requires Authentication (about authentication):
## false

## API rate limited (about rate limiting):
## 1 call per request

## Parameters:
## callback: Optional. Only available for JSON format. If supplied, the response will use the JSONP format with a callback of the given name.
## Example: http://search.twitter.com/search.json?callback=foo&q=twitter
## lang: Optional: Restricts tweets to the given language, given by an ISO 639-1 code.
## Example: http://search.twitter.com/search.atom?lang=en&q=devo
## locale: Optional. Specify the language of the query you are sending (only ja is currently effective). This is intended for language-specific clients and the default should work in the majority of cases.
## Example: http://search.twitter.com/search.atom?q=東京大地震&locale=ja
## max_id: Optional. Returns tweets with status ids less than the given id.
## Example: http://search.twitter.com/search.atom?q=twitter&max_id=1520639490
## q: Optional.  The text to search for.  See the example queries section for examples of the syntax supported in this parameter
## Example: http://search.twitter.com/search.json?&q=twitter
## rpp: Optional. The number of tweets to return per page, up to a max of 100.
## Example: http://search.twitter.com/search.atom?q=devo&rpp=15
## page: Optional. The page number (starting at 1) to return, up to a max of roughly 1500 results (based on rpp * page. Note: there are pagination limits.
## Example: http://search.twitter.com/search.atom?q=devo&rpp=15&page=2
## since: Optional. Returns tweets with since the given date.  Date should be formatted as YYYY-MM-DD
## Example: http://search.twitter.com/search.atom?q=twitter&since=2010-02-28
## since_id: Optional. Returns tweets with status ids greater than the given id.
## Example: http://search.twitter.com/search.atom?q=twitter&since_id=1520639490
## geocode: Optional. Returns tweets by users located within a given radius of the given latitude/longitude.  The location is preferentially taking from the Geotagging API, but will fall back to their Twitter profile. The parameter value is specified by "latitide,longitude,radius", where radius units must be specified as either "mi" (miles) or "km" (kilometers). Note that you cannot use the near operator via the API to geocode arbitrary locations; however you can use this geocode parameter to search near geocodes directly.
## Example: http://search.twitter.com/search.atom?geocode=40.757929%2C-73.985506%2C25km
## show_user: Optional. When true, prepends "<user>:" to the beginning of the tweet. This is useful for readers that do not display Atom's author field. The default is false.
## Example: http://search.twitter.com/search.atom?q=twitterapi&show_user=true
## until: Optional. Returns tweets with generated before the given date.  Date should be formatted as YYYY-MM-DD
## Example: http://search.twitter.com/search.atom?q=twitter&until=2010-03-28
## result_type: Optional. Specifies what type of search results you would prefer to receive.

## Valid values include:

## mixed: In a future release this will become the default value. Include both popular and real time results in the response.
## recent: The current default value. Return only the most recent results in the response.
## popular: Return only the most popular results in the response.
## Example: http://search.twitter.com/search.atom?q=Twitter&result_type=mixed
## Example: http://search.twitter.com/search.json?q=twitterapi&result_type=popular
## Example: http://search.twitter.com/search.atom?q=justin+bieber&result_type=recent

## Usage Notes:
## Query strings should be URL encoded.
## Queries are limited 140 URL encoded characters.
## Some users may be absent from search results.
## The since_id parameter will be removed from the next_page element as it is not supported for pagination. If since_id is removed a warning will be added to alert you.
## This method will return an HTTP 404 error if since_id is used and is too old to be in the search index.
## If you are having trouble constructing your query, use the advanced search form to construct your search, then add the format. For example http://search.twitter.com/search?q=twitter would become http://search.twitter.com/search.json?q=twitter
## Applications must have a meaningful and unique User Agent when using this method. A HTTP Referrer is expected but not required. Search traffic that does not include a User Agent will be rate limited to fewer API calls per hour than applications including a User Agent string.
## After April 1st, 2010 we have a new feature for returning popular tweets in beta. After the beta period the value of result_type=mixed will become the default.

## Example queries:
## Containing a word: http://search.twitter.com/search.atom?q=twitter
## From a user: http://search.twitter.com/search.atom?q=from%3Aal3x
## Replying to a user (tweet starts with @mzsanford): http://search.twitter.com/search.atom?q=to%3Amzsanford
## Mentioning a user (tweet contains @biz): http://search.twitter.com/search.atom?q=%40biz
## Containing a hashtag (up to 16 characters): http://search.twitter.com/search.atom?q=%23haiku
## Combine any of the operators together: http://search.twitter.com/search.atom?q=happy+hour&until=2009-03-24
## Originating from an application: http://search.twitter.com/search.atom?q=landing+source:tweetie

## Search operators:
## Most search operators can be used with API queries.

## Boolean operators:
## OR to combine queries:
## Mentioning @twitterapi OR @twitter: http://search.twitter.com/search.atom?q=%40twitterapi+OR+%40twitter
## Negation: place - in front of the operator.
## Referencing a user but not from that user: http://search.twitter.com/search.atom?q=dougw+-from%3Adougw


library(RCurl) ## to submit the http requests
library(rjson) ## to parse the JSON results
library(foreach) ## to parallelize the post-processing for speed
library(doMC)    ## to parallelize the post-processing for speed
library(stringr)

## FUNCTION: parse.json
## Input: a list of list of JSON objects resulting from a twitter query or queries
## Operation: parses the JSON objects into an R list. Each element of the list
## corresponds to one query. The element contains entries for both the search results
## and ancillary data
## Output: a list of lists
## Note: this assumes that for a list of N queries, the output is a
## list of N lists, of which the elements are JSON objects that can be parsed.

parse.json <- function(infile){

  ## First parse the JSON file to a list
  ## Each element in the list contains the results of one query
  list.json <- mclapply(1:length(infile), function(x){
    lapply(1:length(infile[[x]]), function(y){

      ##print(x)
      out <- try(fromJSON(infile[[x]][[y]]))

      if(grepl("Error", out))
        {
          print(x)
          print(y)
          out <- FALSE
        }
      return(out)

    }
           )


  }
                      )

  return(list.json)

}

## DEPRECATED SEPT 2012
## FUNCTION parse.json.results
## Input: a parsed JSON object or objects in list form
## Operation: converts the "results" portion of each list element to
## list, where each element of the list is a list of all attributes of a single
## search result
## Output: a list of lists
## Note: this assumes that for a list of N queries, the output is a
## list of N lists, of which the elements are JSON objects that can be
## parsed.
## multicore operations are supported to improve on processing time
## for very large datasets

## parse.json.results <- function(in.json){

##   list.results <- lapply(1:length(in.json), function(x){
##     lapply(1:length(in.json[[x]]), function(y){

##       ## Check to make sure theinre are results
##                                   # I CHANGED THE BELOW LINES SO THE ATOMIC VECTOR
##                                   # ERROR WOULDN"T OCCUR: ['results'] instead of $results. - Hillary
##                                   # ... Not sure if the breaks something. ...y]]['results'] always
##                                   # comes out NULL.
##       if(length(in.json[[x]][[y]]['results']) !=0){

##         out <- mclapply(1:length(in.json[[x]][[y]]['results']), function(z){

##           try(unlist(in.json[[x]][[y]]['results'][[z]], recursive=FALSE))

##         }
##                         )


##       }

##     }

##            )
##   }

##                          )

##   return(list.results)

## }

## FUNCTION parse.json.out
## Input: a list of JSON objects resulting from a twitter query
## Operation: parse the JSON object into a list and then convert the list
## into a data frame. This occurs in three steps:
### Parse the json object into a list
### Separate the "results" element from the rest
### Parse the "results" element
### Parse the "other" elements
### Merge the two results
## Output: A data frame with one row for each tweet
## Note: this assumes that for a list of N queries, the output is a
## list of N lists, of which the elements are JSON objects that can be parsed.
## Multicore operation is important here to make processing time
## viable for large datasets.

parse.json.out <- function(infile, results.fields.desired,
                           unique.cand.id, num.cores=2){

  registerDoMC(num.cores)
  list.json <- parse.json(infile)

  ## Generates (2012) this structure:
  ## cand:results_page:results

  out <- foreach(cand.idx=1:length(list.json), .combine=rbind) %:%
    foreach(page.idx=1:length(list.json[[cand.idx]]),
            .combine=rbind,
            .errorhandling="remove") %dopar%{

          cand.id <- unique.cand.id[cand.idx]
          results <- list.json[[cand.idx]][[page.idx]]$results
          results.parsed <- foreach(x=results, .combine=rbind) %do% {

            x[results.fields.desired]

          }
          results.parsed <- as.data.frame(results.parsed)
          results.parsed$unique_cand_id <- cand.id
          return(results.parsed)

    }

  return(out)

}

## DEPRECATED SEPT 2012
## parse.json.out <- function(infile, results.fields.desired,
##                            unique.cand.id, num.cores=2){

##   ## First parse the JSON file to a list
##   ## Each element in the list contains the results of one query
##   ## list.json <- lapply(1:length(infile), function(x){
##   ##   #print(x)
##   ##   fromJSON(infile[[x]])

##   ## }
##   ##                     )


##   registerDoMC(num.cores)

##   list.json <- parse.json(infile)

##   list.results <- parse.json.results(list.json)

##   print(paste("Total number of results", length(list.results)))

##   mat <- matrix(ncol=(length(results.fields.desired) + 1))
##   colnames(mat) <- c(results.fields.desired,
##                      "unique_cand_id"
##                      )

##   obs.counter <- 0

##   ## Core routine that takes the parsed json object (now a list of
##   ## lists) and unlists it into a data frame with the desired fields
##   ## and the input fields for state and candidate.

##   mat <- foreach(i=1:length(list.results), .combine=rbind) %:%
##     foreach(z=1:length(list.results[[i]]), .combine=rbind) %:%
##       foreach(j=1:length(list.results[[i]][[z]]), .combine=rbind) %dopar% {

##         entry <- unlist(list.results[[i]][[z]][[j]])[results.fields.desired]
##         #print(length(entry))
##         #print(class(entry))

##         if(is.null(entry)){
##           entry <- rep(NA, length(results.fields.desired))
##           out <- c(as.character(entry),
##                    unique.cand.id[i]
##                    )
##                                         #break
##         }else{
##           out <- c(as.character(entry),
##                    unique.cand.id[i]
##                    )
##           #print(length(out))
##         }

##                                         #obs.counter <-  obs.counter + 1
##                                         #print(paste("Formatted observation number:", obs.counter))
##         #print(out)
##                                         #as.character(out)
##         out
##       }
##                                         #print(paste("results matrix dim=",dim(mat)))



##   ## Convert to a data frame
##   mat <- as.data.frame(mat)    # HAS TOO MANY ROWS. NAmes vector longer that matrix height.
##   ## print(summary(mat))
##   ## print(names(mat))
##   names(mat) <- c(results.fields.desired,
##                   "unique_cand_id"
##                   )
##   rownames(mat) <- sapply(1:dim(mat)[1], function(x){

##     paste("row", x, sep="")

##   }
##                           )
##   mat.results <<- mat
##   ## End formatting of "results" data
##   #return(out.final)
##   return(mat.results)
## }



## FUNCTION count.query.results
## Input: the "results" part of the json list containing one element for
## each of the queries; each element contains multiple entries, one per returned
## search result
## Operation: counts the number of results for each query in the list
## Output: a vector of counts of search results for each query

## NOTE: To accurately count query results, this must take an input of the form
## parse.json.results(parse.json(x))
count.query.results <- function(list.in){

  lengths <- sapply(1:length(list.in), function(x){

    length(list.in[[x]])

  }
                    )

  #print(lengths)
  #print(sum(lengths))

  return(lengths)
}

## FUNCTION log.likelihood.query
## Input: the "results" part of the json list containing one element for
## each of the queries; each element contains multiple entries, one per returned
## search result
## Operation: Counts the number of search results for each query and then calculates
## the log likelihood of that query relative to all search results.
## Output: a vector of log likelihoods for each query. Returns NA for queries that
## returned no search results.

log.likelihood.query <- function(list.in){

  counts <- count.query.results(list.in)
  ratios <- counts/sum(counts)
  ratios <- ifelse(ratios==0, NA, ratios)
  log.likelihoods <- log(ratios/(1-ratios))

  return(log.likelihoods)
}

## FUNCTION merge.terms
## Input: a data frame with candidate names and other data; and
## a list of search terms. Search terms should be appropriately encoded
## Operation: merges each name with the search terms
## Output: a list, where each element in the list is a vector
## of query terms for a given candidate and all search terms

merge.terms <- function(names, searchTerms){

  out <- lapply(1:dim(names)[1], function(x){

    paste(names$first[x],
          names$last[x],
          searchTerms,
          sep="+"
          )


  }
                )
  return(out)

}

## FUNCTION search.loop
## Input: a list of sets of search terms, the delay interval between searches,
## and a list of names for outfiles
## Operation: runs the search function on each element in the list
## Output: a list of search output results, one for each element in the
## list of sets of search terms

search.loop <- function(terms,
                        delay.interval,
                        since=since,
                        out.file.names=rep(NULL, length(terms))
                        ){

  if(length(terms) != length(out.file.names))
    stop("Must provide a file name for each set of search terms")

  out <- lapply(1:length(terms), function(x){

    search.twitter(terms[[x]],
                   delay.interval=delay.interval,
                   since.date=since,
                   out.file=out.file.names[x]
                   )

  }
                )

  return(out)

}

## FUNCTION search.manager
## Input: a list of sets of search terms, a delay interval between search calls,
## time interval (in seconds) between search repeats, and duration (in minutes)
## for which to run. Ideally, duration/time interval should be a round number
## on which to repeat the search
## Operation: runs the search on all terms on some regular basis; returns only
## search results since the previous search (i.e., only the results in the time
## interval
## Output: saves a set of files on a regular basis with date/time stamps
## containing the output of the search results for that instance of the query

## NOTE: right now, the Twitter API doesn't take "since" inputs at finer
## grain than year-month-day. See the defaults

search.manager <- function(terms,
                           delay.interval=0.5,
                           wait.interval=300, ## wait 5 min
                           duration=24, ## Run for 24hr
                           duration.units="hour"
                           ){

  start.time <- Sys.time()

  ## Get the duration
  if(duration.units=="min")
    duration.secs <- duration*60
  if(duration.units=="hour")
    duration.secs <- duration*3600

  end.time <- start.time+duration.secs

  ## Set the initial "since" value to the previous day
  query.time.prev <- strptime(start.time-(24*3600), format="%Y-%m-%d")

  while(end.time > Sys.time()){

    query.time <- strptime(Sys.time(), format="%Y-%m-%d")
    iteration.results <- search.loop(terms,
                                      delay.interval=delay.interval,
                                      since=query.time.prev
                                      )

    out.file.name <- paste("search.results.",
                           gsub(" ", "", date()),
                           ".RData",
                           sep=""
                           )

    query.time.prev <- query.time
    Sys.sleep(wait.interval)

  }

}


## FUNCTION get.twitter.url()
## Takes a search term and a set of query parameters and returns the
## twitter search results.
## Error handling functions trap http, twitter, and CURL
## errors and re-try the query after a specified wait period
## This is in line with the Twitter API documentation, though
## the handling is a little kludgy.
## Assumes that HTTP errors are returned as HTML files with
## a DOCTYPE field that can be grepped.
get.twitter.url <- function(type, term, rpp, page, since.date,
                            showuser, result.type, delay.interval){



  ## Do NOT want NA requests
  if(!is.na(term)){

    url <- paste("http://search.twitter.com/search.",type,
                 "?q=",term,
                 "&rpp=",rpp,
                 "&page=",page,
                 "&since=", since.date,
                 "&show_user=", showuser,
                                        #"&lang=", lang,
                 "&result_type=", result.type,
                 ## "&geocode=", paste(latitude, longitude, radius, sep=","),
                 sep=""
                 )
                                        #print(url)
                                        #out[[i]] <- try(getURL(url))

    query.out <- try(getURL(url))

    ## Error handling for single requests
    ## Handles either HTTP error headers or the fail whale
    error.counter <- 0

    ## Should plan to handle all of this stuff:
    ## 200 OK: Success!
    ## 304 Not Modified: There was no new data to return.
    ## 400 Bad Request: The request was invalid.  An accompanying error message will explain why. This is the status code will be returned during rate limiting.
    ## 401 Unauthorized: Authentication credentials were missing or incorrect.
    ## 403 Forbidden: The request is understood, but it has been refused.  An accompanying error message will explain why. This code is used when requests are being denied due to update limits.
    ## 404 Not Found: The URI requested is invalid or the resource requested, such as a user, does not exists.
    ## 406 Not Acceptable: Returned by the Search API when an invalid format is specified in the request.
    ## 420 Enhance Your Calm: Returned by the Search and Trends API  when you are being rate limited.
    ## 500 Internal Server Error: Something is broken.  Please post to the group so the Twitter team can investigate.
    ## 502 Bad Gateway: Twitter is down or being upgraded.
    ## 503 Service Unavailable: The Twitter servers are up, but overloaded with requests. Try again later.

    known.errors <- c("DOCTYPE HTML PUBLIC",
                      "whale.png"
                      )
    error.regex <- paste(known.errors,
                         collapse="|"
                         )
    ## Error handling for (in order) Twitter HTTP errors,
    ## the twitter Fail Whale,
    ## and CURL errors from RCURL
    ## Steadily increments the wait time between re-tries,
    ## as recommended in the twitter API documentation
    ## This is a hack.
    while(any(grepl(error.regex, query.out)) |
          class(query.out) == "try-error"
          )
      {
        print(query.out)
        error.counter <- error.counter + 1

        ## Scale the wait based on the error response chain
        Sys.sleep(5 + 5 * (error.counter - 1))
        query.out <- try(getURL(url))

      }

    return(query.out)

  }
}


## FUNCTION search.twitter.pages
## Takes as input a list of search terms, a set
## of query parameters, and a maximum page count
## Formats and submits the twitter query.
## Queries are submitted for pages 1...n, until
## either a Twitter limit or max.pages is reached.

## Output: a list of lists of JSON objects. Each element in the
## master list corresponds to one query in the terms object. Each
## element in the list at that position is one page of queries with
## up to rpp tweets.

## This object is ready to be formatted by parse.results.json().
search.twitter.pages <- function(type="json",
                                 terms,
                                 rpp=100,
                                 delay.interval=2,
                                 since.date="2010-05-27",
                                 lang="en",
                                 showuser="true",
                                 result.type="recent",
                                 write.out=TRUE,
                                 out.file=NULL,
                                 out.interval=50,
                                 max.pages=20){

  out <- list()

  for(i in 1:length(terms))
    {

      if(!is.na(terms[i]))
        {


      page.num <- 1

      query.out <- get.twitter.url(type=type,
                                   term=terms[i],
                                   rpp=rpp,
                                   page=page.num,
                                   since.date=since.date,
                                   showuser=showuser,
                                   result.type=result.type
                                   )
      query.all <- list()
      query.all[[1]] <- query.out
      ## While there are more pages to get, and the user still wants
      ## to get pages
      while(grepl("next_page", query.out) & page.num <= max.pages)
        {

          Sys.sleep(delay.interval)

          page.num <- page.num + 1
          query.out <- get.twitter.url(type=type,
                                       term=terms[i],
                                       rpp=rpp,
                                       page=page.num,
                                       since.date=since.date,
                                       showuser=showuser,
                                       result.type=result.type
                                       )

          query.all[[page.num]] <- query.out

          print(paste("Page num =", page.num))

        }

      out[[i]] <- query.all
      print(i)

      if(i %% out.interval==0 & write.out==TRUE){
        save(out, file=paste("twitter.outfile.",i,".RData", sep=""))
        print(paste("Outfile saved for i=", i))
      }

      ## Pause to avoid throttling by twitter. 1-2s is fine.
      Sys.sleep(delay.interval)


    }

    }

  if(!is.null(out.file))
    {
    save(out, file=out.file)
    return(out)
  }else{
    return(out)
  }
}


## Function count_top_words
## Input: a frequency matrix (should be tF) with columns as terms and
## rows as "documents"; and an N value of "top" terms
## Output: A vector of top terms and frequencies
count.top.words <- function(freq.mat, N)
  {

    freqs <- colSums(freq.mat)
    freqs <- sort(freqs, decreasing=TRUE)

    term.occurance <- sum(freqs, na.rm=TRUE)

    out <- data.frame(names(freqs)[1:N],
                      freqs[1:N],
                      freqs[1:N] / term.occurance
                      )

    names(out) <- c("Term", "Frequency", "Frequency (pct of total)")

    return(out)


  }


## Function to get party info. Short term, probably. Would be more elegant
## to have the cron job incorporate the party data, but I'll likely do that later.
get.party.vector <- function(house.data, candidates){

  names_cron <- ((paste(house.data$first.name, house.data$last.name, house.data$state)))
  # Need first name, last name, and state, so that there are not duplicates.
  all.names <- paste(candidates$first.name, candidates$last.name, candidates$state)
  cron_vs_all_idx <- sapply(names_cron, function(x){
    all.names.match <- which(x==all.names)
    return(all.names.match)
  }
  )
  cron_parties <- candidates$party[as.numeric(cron_vs_all_idx)]
  return(cron_parties)
}

## Function find.chal
## Input: vectors from a data frame of candidate data with the
## following attributes: state, district, and party of candidate
## Output: a vector of indices for which candidates have challengers
## Returns NA if the candidates do not have challengers
find.chal.2 <- function(state, district, party, idx=TRUE)
{
  df <- data.frame(state, district, party)
  df.unique <- unique(df)

  has.chal <- sapply(1:nrow(df.unique), function(x){

    out <- which(state == df.unique$state[x] &
                 district == df.unique$district[x] &
                 party != df.unique$party[x]
                 )

    if(length(out) > 0)
      return(TRUE)
    else
      return(FALSE)

  })

  ## If the user wants the index, return it
  if(idx)
    {
      return(has.chal)

    }else{

      ## Else return the state/dist/party data
      return(df.unique[has.chal,])

    }

}

find.chal.idx <- function(state, district, party){
  df <- data.frame(state, district, party)
  df.unique <- unique(df)

  idx.cand.opp <- foreach(x=1:nrow(df.unique), .combine="rbind") %do% {

    idx.cand <- which(df$state == df.unique$state[x] &
                      df$district == df.unique$district[x] &
                      df$party == df.unique$party[x]
                      )
    idx.chal <- which(df$state == df.unique$state[x] &
                      df$district == df.unique$district[x] &
                      df$party != df.unique$party[x]
                      )

    out <- cbind(idx.cand, idx.chal[1])

    return(out)

  }

  return(idx.cand.opp)

}


find.chal <- function(state, district, party)
  {

    N <- length(state)

    out <- sapply(1:N, function(x){

      idx <- which(state == state[x] &
                   district == district[x] &
                   party != party[x]
                   )

      if(length(idx) > 0)
        return(idx)
      else
        return(NA)

    })

    return(out)

  }


## Replace generic CandDummy labels that worked at the candidate
## level with party-specific dummies that will retain resolution
## at the aggregated district level
## Input: the tweet corpus
##        a vector of party identifiers (assumes D/R) the same length
##        as the corpus
## Output: a new corpus with party-specific candidate dummies
party.dummies <- function(corpus,
                          party){

  party.canddummy <- paste(party,
                           "CandDummy",
                           sep=""
                           )

  opp <- ifelse(party=="D", "R", "D")

  party.oppdummy <- paste(opp,
                          "OppDummy",
                          sep=""
                          )

  corpus.out <- str_replace(corpus,
                            "CandDummy",
                            party.canddummy
                            )
  corpus.out <- str_replace(corpus.out,
                            "OppDummy",
                            party.oppdummy
                            )

  return(corpus.out)

}


## Replace generic CandDummy labels that worked at the candidate
## level with party-neutral differentiated dummies  that will retain resolution
## at the aggregated district level
## Input: the tweet corpus
##        a vector of party identifiers (assumes D/R) the same length
##        as the corpus
##        a vector of 1/0 for whether Cand1 should be democratic
## Output: a new corpus with party-specific candidate dummies
party.neutral.dummies <- function(corpus,
                                  cand1){

  out <- sapply(1:length(corpus), function(x){

    if(cand1[x] == 1)
      {
        corpus.out <- gsub("DCandDummy[A-Z0-9a-z]*",
                           "cand1dummy",
                           corpus[x],
                           fixed=FALSE
                           )

        corpus.out <- gsub("DOppDummy[A-Z0-9a-z]*",
                           "cand1dummy",
                           corpus.out,
                           fixed=FALSE
                           )
        corpus.out <- gsub("RCandDummy[A-Z0-9a-z]*",
                           "cand2dummy",
                           corpus.out,
                           fixed=FALSE
                           )
        corpus.out <- gsub("ROppDummy[A-Z0-9a-z]*",
                           "cand2dummy",
                           corpus.out,
                           fixed=FALSE
                           )
      }else{
        corpus.out <- gsub("RCandDummy[A-Z0-9a-z]*",
                           "cand1dummy",
                           corpus[x],
                           fixed=FALSE
                           )

        corpus.out <- gsub("ROppDummy[A-Z0-9a-z]*",
                           "cand1dummy",
                           corpus.out,
                           fixed=FALSE
                           )

        corpus.out <- gsub("DCandDummy[A-Z0-9a-z]*",
                           "cand2dummy",
                           corpus.out,
                           fixed=FALSE
                           )
        corpus.out <- gsub("DOppDummy[A-Z0-9a-z]*",
                           "cand2dummy",
                           corpus.out,
                           fixed=FALSE
                           )
      }

    return(corpus.out)

  })

  return(out)

}

remove.office.identifiers <- function(corpus.text,
                                      cand1,
                                      office.terms=c("rep",
                                        "congressman")
                                      ){

  corpus.out <- corpus.text
  for(i in 1:length(office.terms))
    {

      corpus.out <- gsub(office.terms[i], "", corpus.out, fixed=TRUE)

    }

  return(corpus.out)

}

party.neutral.party.dummies <- function(corpus.text,
                          cand1
                          ){

  out <- sapply(1:length(corpus.text), function(x){
    if(cand1[x] == 1)
      {

        corpus.out <- gsub("democrat", "party1", corpus.text[x], fixed=TRUE)
        corpus.out <- gsub("republican", "party2", corpus.out, fixed=TRUE)

      }else{

        corpus.out <- gsub("republican", "party1", corpus.text[x], fixed=TRUE)
        corpus.out <- gsub("democrat", "party2", corpus.out, fixed=TRUE)


      }

  })

  return(out)
}


## Aggregate 2-party data to the district level
## Input: a corpus of text data (character vector)
##        a vector of states the same length as the corpus,
##        indicating what state the tweets are from
##        a vector of districts, the same length as the corpus,
##        indicating what district the tweets are from
## NOTE: this implicitly returns vote outcome data as (1) democratic
## vote share and (2) a binary 1/0 dem win/loss vector
aggregate.to.district <- function(corpus, cand.data){

  st.dist <- data.frame(cand.data$state, cand.data$district)

  names(st.dist) <- c("state", "dist")

  st.dist <- unique(st.dist)

  inc <- sapply(1:dim(st.dist)[1], function(x){

    idx <- which(cand.data$state == st.dist$state[x] &
                 cand.data$district == st.dist$dist[x])

    d <- cand.data[idx,]

    has.inc <- sum(as.integer(as.character(d$incumbent))) == 1

    if(has.inc){

      out <- as.character(d$party[d$incumbent == 1])

    }else{

      out <- "N"

    }

    return(out)

  }

                )

  st.dist$incumbent <- inc

  corpus.out <- foreach(i = 1:dim(st.dist)[1], .combine=c) %dopar% {

    idx <- which(cand.data$state==st.dist$state[i] &
                 cand.data$district == st.dist$dist[i]
                 )

    corpus.sub <- paste(corpus[idx], collapse=" ")


  }


  cand.data.out <- cand.data[cand.data$party=="D",]

  out <- list(st.dist,
              cand.data.out,
              corpus.out)

  return(out)


}

aggregate.to.district.wk <- function(corpus, cand.data){

  st.dist <- data.frame(cand.data$state,
                        cand.data$district,
                        cand.data$age.wk
                        )

  names(st.dist) <- c("state", "dist", "age.wk")

  st.dist <- unique(st.dist)

  corpus.out <- foreach(i = 1:dim(st.dist)[1], .combine=c) %dopar% {

    idx <- which(cand.data$state==st.dist$state[i] &
                 cand.data$district == st.dist$dist[i] &
                 cand.data$age.wk == st.dist$age.wk[i]
                 )

    corpus.sub <- paste(corpus[idx], collapse=" ")


  }


  #cand.data.out <- cand.data[cand.data$party=="D",]

  cand.data.out <- cand.data[,c("state",
                                "district",
                                "pctVote"
                                )
                             ]

  out <- list(st.dist,
              cand.data.out,
              corpus.out)

  return(out)


}

## FUNCTION is.spam
is.spam <- function(input.text){




}


## CUSTOM FUNCTIONS FOR THE SUPERLEARNER LIBRARIES

## Do the superlearner here with rf
## Note that svm and randomForrests seem to both do very well
## in OOS prediction (~0.75 - 0.82)
SL.svm.nusvc02 <- function(..., type.class='nu-classification', nu=0.2){
  SL.svm(..., type.class=type.class, nu=nu)
}

SL.svm.nusvc04 <- function(..., type.class='nu-classification',
                           nu=0.4){
  SL.svm(..., type.class=type.class, nu=nu)
}

SL.svm.nusvc06 <- function(..., type.class='nu-classification',
                           nu=0.6){
  SL.svm(..., type.class=type.class, nu=nu)
}

SL.svm.c <- function(..., type.class='C-classification',
                     kernel='linear'){

  SL.svm(..., type.class=type.class, kernel=kernel)

}

SL.svm.c.10 <- function(..., type.class='C-classification',
                        kernel='linear', cost=10
                        ){

  SL.svm(..., type.class=type.class, kernel=kernel, cost=cost)

}

SL.gbmfit.1 <- function (Y.temp, X.temp, newX.temp, family, obsWeights, gbm.trees = 10000,
    ...)
{
    tryCatch(require(gbm), warning = function(...) {
        stop("you have selected gbm as a library algorithm but do not have the gbm package installed")
    })
    if (family$family == "gaussian") {

        fit.gbm1 <- gbm.fit(x=X.temp,
                            y=Y.temp,
                            distribution = "gaussian",
                            n.trees = gbm.trees,
                            interaction.depth = 1,
                            #cv.folds = 5,
                            w = obsWeights,
                            verbose = FALSE
                            )

        best.iter1 <- gbm.perf(fit.gbm1, method = "OOB", plot.it = FALSE)
        out <- predict(fit.gbm1, newdata = newX.temp, best.iter1,
            type = "response")
        fit <- list(object = fit.gbm1, n.trees = best.iter1)
    }

    if (family$family == "binomial") {
        gbm.model <- as.formula(paste("Y.temp~", paste(colnames(X.temp),
            collapse = "+")))
        fit.gbm1 <- gbm(formula = gbm.model, data = X.temp, distribution = "bernoulli",
            n.trees = gbm.trees, interaction.depth = 1, cv.folds = 5,
            keep.data = TRUE, verbose = FALSE, weights = obsWeights)
        best.iter1 <- gbm.perf(fit.gbm1, method = "cv", plot.it = FALSE)
        out <- predict(fit.gbm1, newdata = newX.temp, best.iter1,
            type = "response")
        fit <- list(object = fit.gbm1, n.trees = best.iter1)
    }
    foo <- list(out = out, fit = fit)
    class(foo$fit) <- c("SL.gbm")
    return(foo)
}

SL.gamfit.1 <- function (Y.temp, X.temp, newX.temp, family, obsWeights, deg.gam = 2,
    ...)
{
    tryCatch(require(gam), warning = function(...) {
        stop("you have selected gam as a library algorithm but do not have the gam package installed")
    })

    fit.gam <- gam::gam.fit(x = X.temp, y = Y.temp, family = family,
        control = gam.control(maxit = 50, bf.maxit = 50), weights = obsWeights)
    out <- predict(fit.gam, newdata = newX.temp, type = "response")
    fit <- list(object = fit.gam)
    foo <- list(out = out, fit = fit)
    class(foo$fit) <- c("SL.gam")
    return(foo)
}

SL.svm.polynom.nusvc02 <- function(..., type.class='nu-regression', nu=0.2){
  SL.svm(..., type.class=type.class, nu=nu, kernel="polynomial")
}

## SL.svm.linear <- function(..., kernel="polynomial"){
##   SL.svm(..., kernel=kernel)
## }
## End superlearner library functions


## Custom twitter message grep functions
## Start function definitions
## The following functions abstract the problem of replacing
## proper names for candidates and offices with dummy variables
## to allow comparison across races

remove.all <-
  function(x){

    out <- remove.http(x)
    print(length(out))
    out <- remove.rt(out)
    print(length(out))
    has.out <- remove.usernames(out)
    return(out)

}

remove.http <- function(x){

  corpus <- gsub("\\bhttp://[a-zA-Z0-9./\\+\\?%\\-]*\\b", "", x)
  return(corpus)

}

is.rt <- function(x){

  rt <- grepl("\\b[RTrt]{2}\\s@[a-zA-Z0-9]*", x)
  return(rt)

}

remove.rt <- function(x){

  out <- gsub("\\b[RTrt]{2}\\s@[a-zA-Z0-9]*", "", x, fixed=FALSE)
  return(out)

}

remove.usernames <- function(x){

  out <- gsub("@[a-zA-Z0-9]*", "", x)
  return(out)

}

replace.president <- function(x){

  gsub("\\Qbarack obama\\E|\\Qobama\\E", "PresidentDummy", x)


}

## replace.speaker <- function(x){

##   gsub("\\Qnancy pelosi\\E|\\Qpelosi\\E", "SpeakerDummy", x)

## }

replace.speaker <- function(x){

  gsub("\\Qjohn boehner\\E|\\Qboehner\\E", "SpeakerDummy", x)

}


replace.leader <- function(x){

  gsub("\\Qharry reid\\E|\\Qreid\\E", "LeaderDummy", x)

}

## Special function for replacing the speaker and leader
## names
## This only replaces Pelosi and Reid in references to them
## outside their own district / state
## On the assumption that references outside their own district/state
## are about the office and not the candidate in the race
replace.pol.names <- function(string,
                              state,
                              district,
                              st.speaker,
                              st.leader,
                              dist.speaker
                              ){


  out <- ifelse(state==st.speaker & district==dist.speaker,
                string,
                replace.speaker(string)
                )

  out <- ifelse(state==st.leader,
                out,
                replace.leader(out)
                )

  return(out)


}


replace.candidate <- function(tweet.df, corpus){

  df.regexp  <- paste("\\Q",
                      tolower(tweet.df$first_name),
                      " ",
                      tolower(tweet.df$last_name),
                      "\\E|\\Q",
                      tolower(tweet.df$last_name),
                      "\\E",
                      sep=""
                      )

  corpus.out <- sapply(1:length(df.regexp), function(x){

    gsub(df.regexp[x], "CandDummy", corpus[x])

  })

  return(corpus.out)

}

replace.opponent <- function(tweet.df,
                             first.names,
                             last.names,
                             state,
                             district,
                             chal.idx
                             ){

  corpus <- tweet.df$corpus

  corpus.out <- foreach(i = 1:length(corpus), .combine=c) %dopar% {

      id <- which(first.names == tweet.df$first.name[i] &
                  last.names == tweet.df$last.name[i] &
                  state == tweet.df$state[i] &
                  district == tweet.df$district[i]
                  )

      print(id)
      if(length(id) > 0)
        {
          id.chal <- chal.idx[id]

          chal.first <- tolower(first.names[id.chal])
          chal.last <- tolower(last.names[id.chal])



          regexp <- paste("\\Q",
                          chal.first,
                          " ",
                          chal.last,
                          "\\E|\\Q",
                          chal.last,
                          "\\E",
                          sep=""
                          )

          ## print(paste("Line",
          ##             i,
          ##             "regexp",
          ##             regexp
          ##             )
          ##       )

          out <- gsub(regexp, "OppDummy", corpus[i])
          return(out)
        }else{
          return(corpus[i])
        }

    }
  return(corpus.out)


}

replace.opponent.2 <- function(text,
                               first.name,
                               last.name,
                               chal.idx){

  text.ret <- sapply(1:length(text), function(x){

    chal.first <- tolower(first.name[chal.idx[x]])
    chal.last <- tolower(last.name[chal.idx[x]])

    regexp <- paste("\\Q",
                    chal.first,
                    " ",
                    chal.last,
                    "\\E|\\Q",
                    chal.last,
                    "\\E",
                    sep=""
                    )

    out <- gsub(regexp, "OppDummy", text[x])
    return(out)

  })

  return(text.ret)

}


generate.state.district.code <- function(state, district){

  out <- ifelse(nchar(district) == 1,
                paste(state, "0", district,
                      sep=""
                      ),
                paste(state, district,
                      sep=""
                      )
                )
  return(out)

}


##' Helper function to translate a sparse Matrix into
##' a document-term matrix equivalent to that produced by the tm package.
##' @title SparseToDtm
##' @param sparseM : a sparse Matrix of form dgCMatrix.
##' @param weighting one of weightTf, weightTfIdf, or weightBin
##' @return a simple_triplet_matrix as described in the slam package,
##' with the same dimensions and properties as sparseM.
##' @author Mark Huberty
sparse.to.dtm <- function(sparseM, weighting=weightTf){

  stm <- as.DocumentTermMatrix(sparseM, weighting=weighting)
  return(stm)

}
