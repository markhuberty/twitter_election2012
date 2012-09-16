

generateStats <- function(tweets=master.cron.file, plot=FALSE){
  
  # Tweets per day
  time <- strptime(unlist(tweets$created_at), format="%a, %d %b %Y %H:%M:%S +0000")
  time <- format(time, format="%m-%d-%y")
  per.day <- table(time)
  
  # Tweets per district
  district <- substr(tweets$unique_cand_id, 1,4)
  per.district <- table(district)
  
  # Tweets per candidate
  per.cand <- table(tweets$unique_cand_id)
  # Tweets per candidate per day
  per.cand.day <- table(tweets$unique_cand_id, time)
  # Tweets per candidate per district
  per.cand.district <- table(tweets$unique_cand_id, district)
  
  # Tweets per party
  party <- substr(tweets$unique_cand_id, 6,6)
  per.party <- table(party)
  per.party.day <- table(party, time)
  # Tweets per party per district 
  per.party.district <- table(party, district)
  # Tweets per party per day per district
  # per.day.district.party <- table(time, district, party)
  
  info <- list(as.matrix(per.day),
               as.matrix(per.district),
               as.matrix(per.cand),
               as.matrix(per.cand.day),
               as.matrix(per.cand.district),
               as.matrix(per.party),
               as.matrix(per.party.day),
               as.matrix(per.party.district) 
               # ,(per.day.district.party)
  )
  names(info) <- c("Tweets per Day", "Tweets per District", "Tweets per Candidate",
                   "Tweets per Candidate per day", "Tweets per Candidate per District",
                   "Tweets per Party", "Tweets per Party per Day", "Tweets per Party Per District"
                   # ,"Tweets per Day per District per Party"
  )
  
  
  if(plot==TRUE){
    par(ask=TRUE)
    
    barplot(t(info[[1]]), col="lightblue", cex.names=.8, las=2, main="Tweets per Day", ylab="# of Tweets")
    abline(h=mean(info[[1]]), lwd=2, col="cornflowerblue")
    
    barplot(t(info[[2]]), col="lightblue", axisnames=FALSE, main="Tweets per District", cex.axis=.7,
            ylab="# of Tweets", xlab="Districts")
    abline(h=mean(info[[2]]), lwd=2, col="cornflowerblue")
    
    barplot(t(info[[3]]), col="lightblue", axisnames=FALSE, main="Tweets per Candidate", cex.axis=.8,
            ylab="# of Tweets", xlab="Candidates")
    abline(h=mean(info[[3]]), lwd=2, col="cornflowerblue")
    
    barplot(as.vector(t(info[[6]])), col=c("blue", "white", "red"), cex.names=1, 
            main="Total Tweets per Party", cex.axis=.7, ylab="# of Tweets", names.arg=c("D","I","R"))
    
    barplot(info[[7]], col=c("blue", "white", "red"), cex.names=.8, las=2, main="Tweets per Party Per Day",
            ylab="# of Tweets")
    
    barplot(info[[8]], col=c("blue", "white", "red"), border=c("blue", "white", "red"), axisnames=FALSE,
            main="Tweets per District Per Party", xlab="Districts", ylab="# of Tweets")
    
    par(ask=FALSE)
  }
  
  return(info)
}

stats <- generateStats(master.cron.file, plot=FALSE)
