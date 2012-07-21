# Script to scrape the 2010 Congressional Candidates from opensecrets.org
setwd("~/Documents/Research/Papers/twitter_election2010")
library(RCurl)
library(XML)

## Works as of election 2010
urlroot <- "http://www.opensecrets.org/races/election.php?state="

states <- read.csv("./data/abbr_state.csv")
states <- states[-dim(states)[1],]

#opensecrets.out <- list(rep(NA, dim(states)[1]))

## Loop across the url root and the set of states to get candidates
## for all states
candidates <- list(rep(NA, dim(states)[1]))
for(i in 1:dim(states)[1]){

    candidates[[i]] <- readHTMLTable(paste(urlroot, states$abbrev[i], sep=""),
                                     header=TRUE
                                     )
    #Sys.sleep(5)

    

}

## Strip out only the candidate name
candidates.short <- lapply(1:length(candidates), function(x){

    try(candidates[[x]][[3]])

}
                           )

save(candidates.short, file="./data/candidates2012_all.RData")

## Grab the primary dates from opensecrets
primary.dates <- list(rep(NA, dim(states)[1]))
for(i in 1:dim(states)[1]){

    out <- getURL(paste(urlroot, states$abbrev[i], sep=""),
                  header=TRUE
                  )
    out <- gsub(".*(Primary){1}([:])\\s", "", out)
    out <- gsub("[)].*", "", out)

    out1 <- gsub("(, and only winners are displayed below.)", "", out)
    if(length(grep("(, and only winners are displayed below.)", out))>0)
      {
          out2 <- 1
      }else{
          out2 <- 0
      }
    
    primary.dates[[i]] <- c(out1, out2)
                                        #Sys.sleep(5)

    

}

save(primary.dates, file="primary.dates.RData")


## Define a bunch of functions to process the candidate data
parse.candidates <- function(candidate.list){


    for(j in 1:length(candidate.list)){
        l <- candidate.list[[j]]
        for(i in 1:length(l[,3])){
            
            if(is.na(l[i,3])){

                l[i,3] <- l[i,2]
                l[i,2] <- l[i,1]
                l[i,1] <- l[i-1,1]

            }   
        }

        candidate.list[[j]] <- l
    }

    return(candidate.list)

}

fix.candidate.classes <- function(candidate.list)
  {

      for(i in 1:length(candidate.list))
          {
              l <- candidate.list[[i]]

              for(j in 1:dim(l)[2])
                {

                    l[,j] <- as.character(l[,j])
                    
                }

              candidate.list[[i]] <- l
          }
          
      return(candidate.list)
          
          


  }

mark.incumbent <- function(candidate.list)
  {
      for(i in 1:length(candidate.list))
        {
            l <- candidate.list[[i]]

            l$incumbent <- c(rep(NA, dim(l)[1]))

            for(j in 1:length(l$incumbent))
              {
                  
                  if(length(grep("[*]",l[j,2]))>0)
                    l$incumbent[j] <- 1
                  else
                    l$incumbent[j] <- 0

              }
            candidate.list[[i]] <- l

        }

      return(candidate.list)      

  }


strip.dollar.signs <- function(candidate.list)
  {

      for(i in 1:length(candidate.list))
        {
            
            for(j in 1:dim(candidate.list[[i]])[1])
              {

                  candidate.list[[i]][j,3] <- gsub("[$]", "", candidate.list[[i]][j,3])
                  candidate.list[[i]][j,3] <- gsub(",", "", candidate.list[[i]][j,3])

              }
        }
      return(candidate.list)

  }


restore.classes <- function(candidate.list)
  {
      for(i in 1:length(candidate.list))
          {
              candidate.list[[i]][,1] <- as.factor(candidate.list[[i]][,1])
              candidate.list[[i]][,3] <- as.numeric(candidate.list[[i]][,3])
          }
          
      return(candidate.list)
          
  }


mark.unopposed <- function(candidate.list)
  {

      for(i in 1:length(candidate.list))
          {

              l <- candidate.list[[i]]
              l$unopp <- c(rep(0, dim(l)[1]))
              
              race.levels <- levels(l$Race)

             for(j in 1:length(race.levels))
               {

                   vec <- race.levels[j]==l$Race

                   if(sum(vec)==1)
                     l$unopp[vec] <- 1
                 
                     
               }

              candidate.list[[i]] <- l

              
              

          }
      return(candidate.list)

  }

code.party <- function(candidate.list)
  {

      for(i in 1:length(candidate.list))
        {
            
            l <- candidate.list[[i]]
            l$party <- c(rep(NA, dim(l)[1]))

            for(j in 1:dim(l)[1])
              {
                  ## test.R <- grep("(R)",l[j,2])
                  ## test.D <- grep("(D)",l[j,2])
                  ## test.I <- grep("(I)",l[j,2])
                  
                  ## if(length(test.R)>0)
                  ##   l$party[j] <- "R"
                  ## if(length(test.D)>0)
                  ##   l$party[j] <- "D"
                  ## if(length(test.I)>0)
                  ##   l$party[j] <- "I"

                  l$party[j] <- gsub("^(.*)([(])","", l[j,2])
                  l$party[j] <- gsub("[*)]", "", l$party[j])

              }
            candidate.list[[i]] <- l


        }
      return(candidate.list)



  }

strip.name.extras <- function(candidate.list)
  {

      for(i in 1:length(candidate.list))
          {
              for(j in 1:dim(candidate.list[[i]])[1])
                {
                    candidate.list[[i]][j,2] <- gsub("[(].[)][*]", "",candidate.list[[i]][j,2], perl=TRUE)

                    candidate.list[[i]][j,2] <- gsub("[(].[)]", "",candidate.list[[i]][j,2], perl=TRUE)
                  


                }

                           
          }

      return(candidate.list)
          
  }


fix.col.names <- function(candidate.list)
  {
      for(i in 1:length(candidate.list))
        {

            names(candidate.list[[i]]) <- c("district",
                                           "name.all",
                                           "amt.raised",
                                           "incumbent",
                                           "unopp",
                                           "party"
                                           )

        }

      return(candidate.list)
            
  }

assign.district <- function(candidate.list)
  {

      for(i in 1:length(candidate.list))
          {
              
              candidate.list[[i]]$Race <- gsub("Senate", "S", candidate.list[[i]]$Race)
              candidate.list[[i]]$Race <- gsub("District 0", "", candidate.list[[i]]$Race)
                                               


          }

      return(candidate.list)


  }

parse.first.last.name <- function(candidate.list)
  {
      for(i in 1:length(candidate.list))
        {
            l <- candidate.list[[i]]
            l$first.name <- c(rep(NA, dim(l)[1]))
            l$last.name <- c(rep(NA, dim(l)[1]))
            l$name.all <- trim(l$name.all)
            for(j in 1:dim(l)[1])
              {
                  
                  l$first.name[j] <- gsub("\\s(\\w*)", "", l$name.all[j])
                  l$first.name[j] <- gsub("[.,]", "", l$first.name[j])
                  l$last.name[j] <- gsub("^((\\w*)(\\s)\\b)+", "", l$name.all[j])
                  l$last.name[j] <- gsub("([A-Z])([.])\\s", "", l$last.name[j])
              }
            #candidate.list[[i]] <- 

        }

      return(candidate.list)
  }
                          

    

## Parse the HTML-derived tables in stages based on the functions above
candidates.short <- fix.candidate.classes(candidates.short)
candidates.short.aligned <- parse.candidates(candidates.short)
candidates.short.incumb <- mark.incumbent(candidates.short.aligned)
candidates.short.nodollar <- strip.dollar.signs(candidates.short.incumb)
candidates.short.class <- restore.classes(candidates.short.nodollar)


candidates.opp <- mark.unopposed(candidates.short.class)
candidates.party <- code.party(candidates.opp)
candidate.better.names <- strip.name.extras(candidates.party)
candidates.districts <- assign.district(candidate.better.names)
names(candidates.districts) <- states$abbrev
candidates.districts <- fix.col.names(candidates.districts)

save(candidates.districts, file="candidates.districts.RData")
## Note here: "3" in "party" codes for generic third party, usually Constitution, Tea Party, or similar. "L" is libertarian, "G" is Green, D and R are as would be expected.

## Might not be worth trying to parse the names--too complicated b/c of first,
## middle, last, etc.
##test <- parse.first.last.name(candidates.districts)

sapply(1:length(candidates.districts), function(x){

    dim(candidates.districts[[x]][1])

        }
)

j <- 0
for(i in 1:length(candidates.districts))
  {

      j <- sum(j, dim(candidates.districts[[i]])[1])

  }


## Flip the list into a matrix
mat <- matrix(ncol=6)
states$count.candidates <- NA
colnames(mat) <- names(candidates.districts[[1]])
for(i in 1:length(candidates.districts))
  {

    states$count.candidates[i] <- length(candidates.districts[[i]][[1]])
    mat <- rbind(mat, candidates.districts[[i]])
   


  }

mat <- mat[-1,]
mat$state <- NA
j <- 0
for(i in 1:dim(states)[1])
  {

    if(i==1)
      {
        mat$state[i:states$count.candidates[i]] <- as.character(states$abbrev[i])
      }else{
     mat$state[(sum(states$count.candidates[1:(i-1)])+1):(sum(states$count.candidates[1:i]))] <- as.character(states$abbrev[i])
     }
  }

mat$state <- as.factor(mat$state)

mat$name.all <- gsub("[.]", "", mat$name.all)
mat <- mat[!grepl("All Candidates", mat$party),]
mat <- mat[mat$district != "S",]
mat <- mat[mat$party %in% c("D", "R"),]
mat$district <- gsub("District\\s*", "", mat$district)

write.csv(mat, file="./data/candidates.all.2012.csv")

## Some ancillary code to merge the old candidates file with the new
## one

candidates.all.old <- read.csv("candidates.all.persistent.csv")
names(candidates.all.old)[3] <- "name.all.old"
candidates.all.old <- candidates.all.old[,!(names(candidates.all.old)
                                                 %in% names(mat))]
mat.all <- merge(mat, candidates.all.old,
                 by.x="name.all",
                 by.y="name.all.old",
                 all.x=TRUE,
                 all.y=FALSE
                 )

mat.all$district <- gsub("District", "", mat.all$district)
mat.all$district <- as.factor(mat.all$district)


mat.all.twoparty <- mat.all[mat.all$party %in% c("R", "D"),]

write.csv(mat.all, file="candidates.all.final.csv")
write.csv(mat.all.twoparty, file="candidates.all.final.twoparty.csv")

## NOTE: This script will parse the primary dates out of the HTML
## from opensecrets
##test.date<- gsub(".*(Primary){1}([:])\\s", "", test)
##test.date2 <- gsub("[)].*", "", test.date)
not.yet <- c(8, 9, 50, 40, 12, 30, 33)



