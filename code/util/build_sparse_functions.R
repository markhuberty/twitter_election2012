require(Matrix)
require(foreach)
library(tm)
library(doMC)
library(inline) ## for Rcpp

## New functions for scaling
## This set of functions defines scaling options for the
## term frequencies that are later aggregated.
## Any scaling function can go here
## The function should take as input a vector
## of length (n) that codes for the time variable
## that applies to each message
## They should output a vector of length (n) that
## is scaled as desired.

## FUNCTION scale.linear
## Input:
## vec: the time vector
## params: a vector of length 2 of form (m, b)
## Operation: scales vec on 0,1 based
## on the linear transform mx + b
scale.linear <- function(vec, params){

  stopifnot(length(params) == 2)

  m <- params[1]
  b <- params[2]


  vec.scale <- m * vec + b
  #vec.scale <- vec.scale / max.scale

  return(vec.scale)

}

## FUNCTION scale.quad
## Input:
## vec: the time vector
## params: a vector of length 4 of form (a, b, c, n)
## Operation: scales vec on 0,1 based
## on the quadratic transform a + bx + cx^n
scale.quad <- function(vec, params){

  stopifnot(length(params) == 4)

  a <- params[1]
  b <- params[2]
  c <- params[3]
  n <- params[4]

  vec.scale <- a + b * vec + c * vec^n

  #vec.scale <- vec.scale / max(vec.scale)

  return(vec.scale)

}

## FUNCTION scale.sigmoid
## Input:
## vec: the time vector
## params: a vector of length 1 of form (a)
## Operation: scales vec on 0,1 based
## on the linear transform x / (1 + exp(-a*(x=median(x))))
## Note that this means that the sigmoid curve
## is centered at 0.5
scale.sigmoid <- function(vec, params){

  stopifnot(length(params) == 1)

  a <- params[1]

  mid.point <- median(vec)

  vec.scale <- 1 / (1 + exp(-a * (vec - mid.point)))

  #vec.scale <- vec.scale / max(vec.scale)

  return(vec.scale)

}

scale.uniform <- function(vec, params=NULL){

  return(rep(1, length(vec)))

}

## End scaling functions

## FUNCTION scale.weights.by.time
## Input:
## time.var: a time variable at the level of specificity
## desired for the scaling
## sparsem.mat a sparse matrix whose data are the data
## to be scaled
## scale.fun: the function to use for scaling
## scale.params: a vector of parameters for the scaling
## function--see the function def for the order of arguments

## Inline C code for scaling. Cannot do column multiplication
## b/c of memory issues. This inlines C code to do it fast w/
## minimal memory consumption.
## Note the indexing nuance: i is a 0-based triplet row index vector
## from Matrix; normally would need to add 1 but scale_vec indexed
## starting at 0 inside the C code. So this is fine.
rcpp.scale.src <- "
  int row_idx;
  for(int row=0; row < *nrow; row++) {

   row_idx = i[row];
   x[row] = x[row] * scale_vec[row_idx];

}"
rcpp.scale.sig <- signature(i="integer", nrow="integer", x="numeric", scale_vec="numeric")
c.scale <- cfunction(sig=rcpp.scale.sig,
                     body=rcpp.scale.src,
                     convention=".C"
                     )

scale.weights.by.time <- function(time.var,
                                  sparse.mat,
                                  scale.fun="scale.linear",
                                  scale.params=c(1,0)
                                  ){
  ## Convert the matrix for efficiency

  stopifnot(length(time.var) == nrow(sparse.mat))

  ## Transform the time var into a 1:n index
  ## Note that this is weird b/c of how the weights are totally
  ## unstable
  scale.period <-
    as.integer(as.Date("2012-11-06")) - min(as.integer(time.var))
  x <- as.integer(time.var) - min(as.integer(time.var)) + 1
  x <- x / scale.period

  fun <- match.fun(scale.fun)
  ## Scale with the weight function
  scale.factor <- fun(x, scale.params)
  print("Scaling factor computed")
  ## Define the RCPP function

  sparse.mat <- as(sparse.mat, "dgTMatrix")
  ## print(length(sparse.mat@x))
  scaled.values <- c.scale(sparse.mat@i, length(sparse.mat@i), sparse.mat@x, scale.factor)$x
  ## print(length(scaled.values))
  sparse.mat@x <- scaled.values
  sparse.mat <- as(sparse.mat, "dgCMatrix")

  ## Rejigger this so it isn't a memory hog
  ## for(i in 1:nrow(sparse.mat))
  ##   {
  ##     sparse.mat[i,] <- sparse.mat[i, ] * scale.factor[i]
  ##     sparse.mat <- drop0(sparse.mat)
  ##   }
  print("Scaling complete")
  print(class(sparse.mat))
  return(sparse.mat)


}

## Aggregates a doc-term matrix by summing the weights for each
## feature (column) by a grouping factor that selects 1 or more rows.
## If binary=TRUE, then converts the final matrix to a 1/0 coding for
## presence rather than frequency
## Returns a doc-term matrix with unique(fac) rows, and the factor labels
aggregate.by <- function(fac, sparse.mat, binary=FALSE){
  print("Aggregating")
  unique.factor <- unique(fac)

  ## Convert to triplet for efficiency
  sparse.mat <- as(sparse.mat, "dgTMatrix")
  mat.out <- foreach(i=1:length(unique.factor), .combine="rbind") %do% {

    idx.factor <- which(fac == unique.factor[i])

    mat.sum <- sparse.mat[idx.factor,]

    if(is.numeric(mat.sum))
      out <- mat.sum
    else
      out <- colSums(mat.sum, na.rm=TRUE)

    return(out)

  }

  if(binary)
    mat.out <- mat.out > 0

  ## Convert back for consistency
  mat.out <- as(mat.out, "dgCMatrix")
  mat.out <- list(unique.factor, mat.out)
  return(mat.out)

}

## Takes as input a disaggregated doc-term matrix
## and an aggregation factor, and returns a doc-term matrix
## aggregated by agg.fac, of the type produced by aggregate.by().
## If scale=TRUE, scales the frequency data _before_ aggregation,
## such that the summed terms in the aggregated spreadsheet reflect
## the weighting.
## NOTE that the scaling here may not be appropriate if the
## aggregation
## factor has a time element (i.e., aggregate by place and time).

## tdm must be a doc-term matrix of the form output by the tm package
generate.sparse.tdm <- function(tdm,
                                agg.fac,
                                initial.threshold,
                                final.threshold,
                                col.names,
                                scale=FALSE,
                                time.var=NULL,
                                scale.fun=NULL,
                                scale.params=NULL,
                                sparse.filter=TRUE,
                                tfidf.filter=FALSE,
                                tfidf.threshold=0.01
                                ){

  sparse.corpus <- sparseMatrix(i=tdm$i,
                                j=tdm$j,
                                x=tdm$v,
                                dims=c(tdm$nrow,
                                  tdm$ncol)
                                )
  print(dim(sparse.corpus))
  ## row.count <- length(unique(agg.fac))

  if(sparse.filter)
    {
      word.count <- colSums(sparse.corpus)
      ## Sparseness computed
      idx.to.keep <- which(word.count >= initial.threshold)

      sparse.corpus <- sparse.corpus[,idx.to.keep]
      col.names <- col.names[idx.to.keep]
      print(dim(sparse.corpus))
    }

  ## NOTE: faster to filter on sparseness first; dim reduction
  ## helps a lot
  if(tfidf.filter)
    {
      print("Filtering on tfidf")
      sparse.corpus <- select.tfidf(sparse.corpus, tfidf.threshold, col.names)
      col.names <- colnames(sparse.corpus)
    }


  if(scale)
    sparse.corpus <- scale.weights.by.time(time.var=time.var,
                                           sparse.mat=sparse.corpus,
                                           scale.fun=scale.fun,
                                           scale.params=scale.params
                                           )

  print("Sparseness computed")
  print(dim(sparse.corpus))

  tdm.agg <- aggregate.by(agg.fac,
                          sparse.corpus
                          )

  row.names <- tdm.agg[[1]]

  tdm.agg <- tdm.agg[[2]]

  ## Finalize the sparseness
  ## Note that the sparseness above was largely a way to
  ## handle the compute problem by throwing out really sparse stuff
  ## This actually makes sure that sparseness is equiv to what's in
  ## the tm package
  if(sparse.filter)
    {
      sparseness <- colSums(tdm.agg > 0) / nrow(tdm.agg)
      to.save <- which(sparseness > final.threshold)
      tdm.agg <- tdm.agg[,to.save]
      col.names <- col.names[to.save]
    }

  rownames(tdm.agg) <- row.names
  colnames(tdm.agg) <- col.names

  return(tdm.agg)

}


## FUNCTION weight.tfidf
## Translates a document-term matrix w/ term-frequency dat
## into a tf-idf matrix
## tf idf translation goes as:
## (word freq in document) * log(doc count / word occurance in all
## docs)
## Assumes documents are rows and features are columns
## The use of the +1 modifier ensures real-valued returns
## for zero-occurrance features
weight.tfidf <- function(mat){

  doc.word.counts <- rowSums(mat)
  docs <- nrow(mat)
  word.doc.freqs <- colSums(mat > 0) + 1
  idf <- log(docs / word.doc.freqs)

  tfidf <- sapply(1:ncol(mat), function(x){
    this.col <- mat[,x]
    idf <- idf[x]
    tf <- this.col / doc.word.counts
    out <- tf * idf
    return(mean(out, na.rm=TRUE))

  })
  ## idf <- log(nrow(mat) / (colSums(mat > 0) + 1))
  ## tf <- mat / doc.word.counts
  ## tfidf <- t(t(tf)*idf)
  return(tfidf)

}

## FUNCTION select.tfidf
## Takes a document-term matrix of term frequencies
## Selects features based on mean tfidf for each term
## Returns a doc-term matrix of terms whose mean tfidf
## was > a supplied threshold value

select.tfidf <- function(mat, threshold, col.names){

  stopifnot(threshold > 0)

  mean.tfidf <- weight.tfidf(mat)
  ## Take averages by column
  print(summary(mean.tfidf))

  idx <- which(mean.tfidf > threshold)

  print("N terms passed idf filter:")
  print(length(idx))

  mat.out <- mat[,idx]
  colnames(mat.out) <- col.names[idx]

  return(mat.out)


}
