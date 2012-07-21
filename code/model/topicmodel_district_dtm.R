## Code to run the dynamic topic modeler from Blei et al
## on the time-based Twitter election corpus
## Begun 2 October 2011

## Code to model the topics by district
## Begun 22 September 2011
## Mark Huberty

if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/twitter_election2010")
  }else{
    setwd("~/Documents/twitter_election2010")
  }

source("./code/dtm_wrapper.R")
#load("./data/corpus.district.wkly.RData")
load("./data/tdm.sparse.2.RData")
library(tm)
library(topicmodels)
library(RWeka)
library(foreach)
## Here, load the time-based corpus

## Then run the file creation code

corpus.district.tdm.mat <- tdm.sparse
rm(tdm.sparse)
print(dim(corpus.district.tdm.mat))
gc()

## Try to drop some stuff
idx.drop <- which(colnames(corpus.district.tdm.mat) %in%
                  c("rep dcanddummy",
                    "rep rcanddummy",
                    "congressman dcanddummy",
                    "congressman rcanddummy",
                    "rcanddummy congress",
                    "dcanddummy congress",
                    "candidate dcanddummy",
                    "candidate rcanddummy",
                    "congresswoman dcanddummy",
                    "congresswoman rcanddummy",
                    "cliff lee"
                   )
                  )
corpus.district.tdm.mat <- corpus.district.tdm.mat[,-idx.drop]

idx.zeros <- which(rowSums(corpus.district.tdm.mat) == 0)

corpus.district.tdm.mat <- corpus.district.tdm.mat[-idx.zeros,]

ages <- gsub("^[A-Z0-9.]{5,6}([0-9]{1}$)", "\\1", rownames(corpus.district.tdm.mat))
ages <- as.integer(ages)

corpus.district.tdm.mat <-
  corpus.district.tdm.mat[order(ages),]

ages <- sort(ages)

save(corpus.district.tdm.mat,
     ages,
     file="./data/dtm_input_source_files.RData"
     )

create.dtm.input.files(corpus.district.tdm.mat,
                       ages,
                       file.root="twitter",
                       file.path="./data/"
                       )

flag.terms <- c("ntopics", "mode", "rng_seed", "initialize_lda",
                "top_chain_var", "alpha", "lda_sequence_min_iter",
                "lda_sequence_max_iter", "lda_max_em_iter",
                "outname"
                )
flag.vals <- c(10, "fit", 0, "true", 0.005, 0.01, 6, 50, 10,
               "/home/markhuberty/Documents/twitter_election2010/data/dtm"
               )

flags <- cbind(flag.terms,
               flag.vals
               )

run.dtm("/home/markhuberty/admin/dtm_release/dtm",
        file.path="/home/markhuberty/Documents/twitter_election2010/data",
        file.root="twitter",
        flags=flags,
        model="dtm",
        verbose=FALSE
        )

save(corpus.district.tdm.mat,
     ages,
     file="./data/dtm_input_source_files.RData"
     )
## Then run the dtm code

## Then load and analyze

