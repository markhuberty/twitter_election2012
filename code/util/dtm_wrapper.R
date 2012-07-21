## Functions to provide a wrappter to the C++ dtm code from
## Blei & Garrish

## FUNCTION write.mult.file
## In: a document-term matrix w/ term-frequency counts
## (see the tm package for how to create this).;
## a vector of time stamps in the same order as the documents in the
## doc-term matrix; and output locations (strings) for the
## resulting files
## Out: a flat file, written to a specified location,
## in the format specified in the sample.sh script included
## with the dtm C++ code

write.mult.file <- function(doc.term.mat,
                           time.stamps,
                           file.root,
                           file.path
                           ){

  ## Make sure that the documents are row-sorted
  ## by time
  doc.term.mat <- doc.term.mat[order(time.stamps),]
  
  ## Count the number of unique words in each document
  unique.word.counts <- rowSums(doc.term.mat > 0,
                                na.rm=TRUE
                                )

  ## Represent each document as a word.index:word.count
  ## vector
  ## Format for each row for dtm should be:
  ## unique.word.count index1:count1 index2:count2 ...
  
  sparse.word.representation <-
    lapply(1:nrow(doc.term.mat), function(i){

      vec <- doc.term.mat[i,]

      count.vec <- sapply(1:length(vec), function(x){

        ## Sparse representation--don't use zeros
        if(vec[x] > 0)
          {
            ## Note that this data should be zero-indexed, hence x-1
            out <- paste(x-1, ":", vec[x], sep="")
            return(out)
          }else{
            return(NULL)
          }

        ## This returns NULLs right now if vec[x] =0...

      })

      count.vec <- unlist(count.vec)
      count.vec <- count.vec[!is.null(count.vec)]
       
      ## Stitch the entire set of data together
      row.out <- paste(unique.word.counts[i],
                       paste(count.vec, collapse=" "),
                       sep=" "
                       )
      #print(row.out)

      return(row.out)

    }
           )

  full.filename <- paste(file.path, file.root, "-mult.dat", sep="")
  conn <- file(full.filename, open="w")
  for(i in 1:length(sparse.word.representation))
    {
      writeLines(sparse.word.representation[[i]], con=conn)
      
    }
  close(conn)

  print("Total words\n")
  print(sum(unique.word.counts, na.rm=TRUE))
  
  return("Done")

}

## FUNCTION: write.seq.file
## Writes the seq file required by the dtm algorithm
## Inputs:
## time.stamps: a list of time stamps for the documents that
##      the dtm algorithm will process
##      these should be in an R-sortable format, such as Date or POSIX
##      they can also be integer years
## file.root: a root filename for the foo-seq.dat file that
## will result
## file.path: a filepath for where to put the output file

write.seq.file <- function(time.stamps,
                           file.root,
                           file.path
                           ){

  unique.time.stamps <- sort(unique(time.stamps))

  count.time.stamps <- sapply(unique.time.stamps, function(x){

    sum(time.stamps == x)

  })

  full.filename <- paste(file.path, file.root, "-seq.dat", sep="")
  conn <- file(full.filename, open="w")
  writeLines(as.character(length(unique.time.stamps)), con=conn)
  for(i in 1:length(unique.time.stamps))
      {
        
        writeLines(as.character(count.time.stamps[i]), con=conn)

      }
  close(conn)
  return("Done")
  
}

## FUNCTION: write.label.file
## Writes the label file required by the dtm algorithm
## Inputs:
## labels: 
## time.stamps: a list of time stamps for the documents that
##      the dtm algorithm will process
##      these should be in an R-sortable format, such as Date or POSIX
##      they can also be integer years
## file.root: a root filename for the foo-seq.dat file that
## will result
## file.path: a filepath for where to put the output file

write.label.file <- function(labels,
                             time.stamps,
                             file.root,
                             file.path){

  labels <- labels[order(time.stamps)]

  full.filename <- paste(file.path, file.root, "-lab.dat", sep="")
  conn <- file(full.filename, open="w")
  for(i in 1:length(labels))
    {

      writeLines(as.character(labels[i]), con=conn)
      
    }

  close(conn)
  return("Done")


}

## FUNCTION create.dtm.input.files
## This is a wrapper for the two file creation functions.
## It produces a set of -mult.dat and -seq.dat files for
## subsequent use in run.dtm.
## It also returns input values that can be used in the run.dtm
## code
## Inputs:
## doc.term.mat: a matrix with rows as documents and columns as word
##      frequency counts
## time.stamps: time stamps for each document, in the same order as
##      the rows in doc.term.mat
## file.root: the file root to be used to create foo-mult.dat and
##      foo-seq.dat

create.dtm.input.files <- function(doc.term.mat,
                                   time.stamps,
                                   file.root,
                                   file.path,
                                   labels=NULL,
                                   write.labels=FALSE
                                   ){

  stopifnot(nrow(doc.term.mat) == length(time.stamps))
            
  write.mult.file(doc.term.mat=doc.term.mat,
                  time.stamps=time.stamps,
                  file.root=file.root,
                  file.path=file.path
                  )

  write.seq.file(time.stamps=time.stamps,
                 file.root=file.root,
                 file.path=file.path
                 )

  if(write.labels)
    {
      write.lab.file(time.stamps=time.stamps,
                     file.root=file.root,
                     file.path=file.path
                     )
    }
  
  out <- c(file.path, file.root)
  names(out) <- c("file.path", "file.root")

  return(out)


}

## FUNCTION run.dtm
## This runs the actual DTM script. 
## Takes as input the following objects:
## path.to.main: the filepath to the `main` executable in the dtm
##      library
## file.path: the path to the -mult and -seq files required
##      as inputs to dtm
## file.root: the name of the file prefix for foo-mult.dat and
##      foo-seq.dat
## flags: a 2-column data frame with flags to set for the dtm
##      algorithm.
##      Column1 should be a character vector of flags without the -- prefix
##      Column2 should be the values for the flags in column 1
##      Run ./main --help to get a full set of flags.
##      If this is NULL, the defaults are used.
## verbose: determines if the command-line output is printed to the
##      R console

## verbose determines whether console output is printed to the R
## console or Rout file.

run.dtm <- function(path.to.main="./",
                    model="dtm",
                    file.path=NULL,
                    file.root=NULL,
                    flags=NULL,
                    verbose=FALSE
                    ){

  stopifnot(!is.null(file.path) &
            !is.null(file.root)
            )
                     

  main.call <- paste(path.to.main,
                     "main",
                     sep="/"
                     )

  model.call <- paste("--model=",
                      model,
                      sep=""
                      )

  corpus.root <- paste(file.path,
                       file.root,
                       sep="/"
                       )
  
  corpus.call <- paste("--corpus_prefix",
                       corpus.root,
                       sep="="
                       )

  main.call <- paste(main.call,
                     model.call,
                     corpus.call,
                     sep=" "
                     )                     
                     
                     
  if(!is.null(flags))
    {
      flag.call <- sapply(1:nrow(flags), function(x){

        paste("--",
              flags[x,1],
              "=",
              flags[x,2],
              sep=""
              )

      }
                          )

      flag.call <- paste(flag.call, collapse=" ")

      main.call <- paste(main.call,
                         flag.call,
                         sep=" "
                         )
      
    }

  print(paste("Calling dtm with this input: ", main.call,
              sep=" ")
        )
  
  system(main.call, intern=verbose)

  return("Done")
     
}


## Usage
## source("dtm_wrapper.R")
## dtm.inputs <- create.dtm.input.files()
## run.dtm(...,
##         file.path=dtm.inputs$file.path,
##         file.root=dtm.inputs$file.root,
##         ...
##         )
## see sample.sh for further information on how to load the
## resulting files


################################################
## Functions for processing output

## Function process.word.dist
## Handles the topic-???-var-e-log-prob.dat files
## that the dtm model generates for the word distributions
## at for topic ??? across time

## Inputs:
## ntopics: number of topics specified in the dtm model
## nperiods: number of time periods specified in the dtm model
## output.path: the location of the dtm output files
## word.list: the term list, in the same order as provided to the dtm
## program
## prob: boolean, whether the matrix should be returns as the log-prob
## (FALSE) or the true probability (TRUE) of term X in topic Y and
## time Z

process.word.dist <- function(ntopics,
                              nperiods,
                              output.path,
                              word.list,
                              prob=TRUE
                              ){

  require(stringr)
  
  topic.strings <- seq(from=0,
                       to=ntopics-1,
                       by=1
                       )
  
  topic.strings <- str_pad(topic.strings,
                           width=3,
                           side="left",
                           pad="0"
                           )

  file.strings <- paste(output.path,
                        "/",
                        "topic-",
                        topic.strings,
                        "-var-e-log-prob.dat",
                        sep=""
                        )

  list.out <- lapply(file.strings, function(x){

    vec <- scan(x)
    mat <- matrix(vec, ncol=nperiods, byrow=TRUE)

    if(prob)
      mat <- exp(mat)

    rownames(mat) <- word.list
    return(mat)

  })
  ## a = scan("topic-002-var-e-log-prob.dat")
  ## > b = matrix(a, ncol=10, byrow=TRUE)

  return(list.out)
}

## FUNCTION process.topic.mixtures
## Handles the gam file that provides the topic mixtures for each doc

## Inputs:
## ntopics number of topics in the dtm model
## output.path: location of the dtm output files

## Output:
## A matrix of proportions, dimensions documents * topics

process.topic.mixtures <- function(ntopics,
                                   output.path
                                   ){

  gamfile <- paste(output.path,
                   "gam.dat",
                   sep="/"
                   )

  vec <- scan(gamfile)
  mat <- matrix(vec, ncol=ntopics, byrow=TRUE)

  rs.mat <- rowSums(mat)

  e.theta <- mat / rs.mat

  return(e.theta)

}

## FUNCTION: top.terms.topics
## Purpose: returns the top N terms by time period for each topic
## Input: a word distributoin as output from process.word.dist()
##        a desired top N terms
## Output: a list of top terms by time period by topic

top.terms.topics <- function(word.dist, n.terms){


  list.out <- lapply(word.dist, function(x){

    words.out <- foreach(i=1:ncol(x), .combine="cbind") %do% {

      mat <- x[order(x[,i], decreasing=TRUE),]

      words.out <- rownames(mat)[1:n.terms]
      probs.out <- mat[1:n.terms, i]
      
      return(words.out)


    }

    probs.out <- foreach(i=1:ncol(x), .combine="cbind") %do% {

      mat <- x[order(x[,i], decreasing=TRUE),]
      
      probs.out <- mat[1:n.terms, i]
            
      return(probs.out)


    }


    return(list(words.out, probs.out))

  })

  return(list.out)

}

doc.topic.period <- function(etheta, N){

  vec.out <- sapply(1:nrow(etheta), function(x){

    vec <- sort(etheta[x,], decreasing=TRUE)

    vec <- vec[1:N]

    out <- sapply(vec, function(y){as.integer(which(etheta[x,] == y))[1]}) 

    return(out)
  })

  return(vec.out)

}

## FUNCTION count.docs.by.period.topic
## Counts documents by period and topic based on the output of a topic
## model
## Inputs:
##   etheta: The matrix as output by process.topic.mixtures
##   ages: The age of each document input to DTM
##   topic.names: a character vector of topic names, with as many
##     names as topics codified in etheta
count.docs.by.period.topic <- function(etheta, ages, topic.names){

  doc.topic.vec <- doc.topic.period(etheta, 1)

  unique.ages <- sort(unique(ages))

  ntopics <- ncol(etheta)
  
  out <- foreach(i=1:length(unique.ages), .combine="rbind") %do% {

    sapply(1:ntopics, function(x){

      sum(ages == unique.ages[i] &
          doc.topic.vec == x
          )

    })

  }

  colnames(out) <- topic.names
  return(out)

}



## FUNCTION plot.dtm.terms
## Input: a list of top terms from a dtm model by period,
## as output from top.terms.topics()
## Optional period labels
## A ColorBrewer palette
## Output: a ggplot object that plots the top terms by period
## with the size of the term 
plot.dtm.terms <- function(top.terms.periods, 
                           period.labels=NULL,
                           topic.labels=NULL,
                           palette="PuRd"){
  require(ggplot2)
  require(RColorBrewer)

  n.topics <- length(top.terms.periods)
  n.terms <- nrow(top.terms.periods[[1]][[1]])
  n.periods <- ncol(top.terms.periods[[1]][[1]])

  if(is.null(period.labels))
    period.labels <- paste("P", 1:n.periods, sep="")

  if(is.null(topic.labels))
    topic.labels <- paste("T", 1:n.topics, sep="")
  ## Basic idea: use ggplot, plot the terms
  ## on a grid of topic * time, size them internally by prob

  words.weights <- foreach(i=1:n.topics, .combine=rbind) %do%{

    mat.words <- top.terms.periods[[i]][[1]]
    mat.weights <- top.terms.periods[[i]][[2]]

    for(j in 1:ncol(mat.words))
      {

        vec <- mat.words[,j]
        vec.sort <- mat.weights[,j] / max(mat.weights[,j])

        vec <- vec[order(vec.sort)]
        vec.sort <- sort(vec.sort)
        
        mat.words[,j] <- vec
        mat.weights[,j] <- vec.sort

      }

    mat.words <- data.frame(topic.labels[i],
                            1:n.terms,
                            mat.words
                            )
    mat.weights <- data.frame(topic.labels[i],
                              1:n.terms,
                              mat.weights
                              )
    names(mat.words) <- names(mat.weights) <- c("Topic",
                                                "Term",
                                                period.labels
                                                )

    mat.words <- melt(mat.words, id.var=c("Topic", "Term"))
    mat.weights <- melt(mat.weights, id.var=c("Topic", "Term"))

    names(mat.words) <- c("Topic", "Term", "Period", "Word")
    mat.words$Weight <- mat.weights$value

    return(mat.words)

  }

  words.weights$X <- 0
  plot.out <- ggplot(words.weights,
                     aes(x=X,
                         y=Term,
                         size=Weight,
                         label=Word
                         )
                     ) +
                       geom_text() +
                         facet_grid(Topic ~ Period) +
                           scale_x_continuous(limits=c(-2,2)) +
                             scale_y_continuous(limits=c(0, n.terms+1)) +
                               scale_size("", to=c(1, 4)) +
                                 opts(axis.text.y = theme_blank(),
                                      axis.text.x = theme_blank(),
                                      legend.position="none") 

  return(plot.out)
  
}
 
