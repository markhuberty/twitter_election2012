## Code to reformat the candidate/district files to a wide format
## that has the district and candidate data in a single row
candidates <- read.csv("./data/candidates.csv", stringsAsFactors=FALSE)
districts <- read.csv("./data/districts.csv", stringsAsFactors=FALSE)

## Handle the candidates file
## state_dist district state state_id cand_d cand_r party_incumb
## rating


generate.cand.names <- function(candidates, districts){

  parties <- unique(candidates$party)
  rd.names <- sapply(1:nrow(districts), function(x){

    id <- districts$state_dist[x]
    party.names <- lapply(parties, function(p){

      p.name <- candidates[candidates$state_dist==id &
                           candidates$party==p,
                           c("name", "first_name", "last_name")
                           ]

      if(any(sapply(p.name, function(x) !length(x))))
        p.name <- rep(NA, 3)

      return(p.name)

    })

    out <- unlist(party.names)
    return(out)

  })
  

  rd.names <- t(rd.names)
  col.names <- paste(parties, c("name", "first_name", "last_name"), sep="_")
  colnames(rd.names) <- col.names
  
  rd.names <- as.data.frame(rd.names)
  df.out <- cbind(districts, rd.names)
  return(df.out)

}

test <- generate.cand.names(candidates, districts)
head(test)
