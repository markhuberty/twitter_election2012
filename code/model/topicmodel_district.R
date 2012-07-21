## Code to model the topics by district
## Begun 22 September 2011
## Mark Huberty

if(grepl("mac.binary", .Platform$pkgType, fixed=TRUE))
  {
    setwd("~/Documents/Research/Papers/twitter_election2010")
  }else{
    setwd("~/Documents/twitter_election2010")
  }


library(tm)
library(topicmodels)
library(RWeka)


load("./data/corpus.district.RData")


my.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2,
                                                           max = 2)
                                           )

corpus.district.tdm <- TermDocumentMatrix(corpus.district,
                                          control=list(tokenize=my.tokenizer,
                                            weighting=weightTf)
                                          )

## Then strip out the sparseness. For this dataset,
## 0.95 yields about 1411 terms
sparseness <- 0.99

corpus.district.tdm.sparse <- if(sparseness < 1){
  removeSparseTerms(corpus.district.tdm,
                    sparseness)
}else{
  corpus.district.tdm
}


corpus.district.tdm.mat <- t(as.matrix(corpus.district.tdm.sparse))
print(dim(corpus.district.tdm.mat))

## Try to drop some stuff
idx.drop <- which(colnames(corpus.district.tdm.mat) %in%
                  c("rep dcanddummy",
                    "rep rcanddummy",
                    "support dcanddummy",
                    "support rcanddummy",
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

## Begin topic models

k <- 5
seed <- 3423

tm.lda.district <- LDA(corpus.district.tdm.mat,
                       k=k,
                       control=list(seed=seed)
                       )

terms.lda.district <- terms(tm.lda.district, 5)
topics.lda.district <- topics(tm.lda.district)

table(as.factor(topics.lda.district),
      as.factor(cand.data.dist$d.victory)
      )


control_CTM_VEM <- 
  list(estimate.beta = TRUE, 
       verbose = 0, prefix = tempfile(), save = 0, 
       seed = seed, 
       var = list(iter.max = 500, tol = 10^-6), 
       em = list(iter.max = 1000, tol = 10^-4), 
       initialize = "random", 
       cg = list(iter.max = 500, tol = 10^-5)
       )

tm.ctm.district <- CTM(corpus.district.tdm.mat,
                       k=k,
                       control=control_CTM_VEM
                       )
terms.ctm.district <- terms(tm.ctm.district, 5)
topics.ctm.district <- topics(tm.ctm.district)

table(as.factor(topics.ctm.district),
      as.factor(cand.data.dist$d.victory)
      )

## Should redo this w. max loglik / min perplex on held-out
## sample. See the leghist stuff for details.


## Now want to plot the terms by lat/long of the centroids

library(sp)
library(maptools)
library(ggplot2)
library(RColorBrewer)
cd.map <- readShapePoly("./data/cd99_110.shp")
cd.map$sortid <-  sapply(slot(cd.map, "polygons"), function(x) slot(x, "ID"))
cd.map$sortid <- as.numeric(cd.map$sortid)

## PLOT DISTRICT DATA ##
orig.cd.table <- as(cd.map, "data.frame")
orig.cd.table$STATE <- as.integer(as.character(orig.cd.table$STATE))
orig.cd.table$NAME <- ifelse(orig.cd.table$NAME=="One", 1, orig.cd.table$NAME)
orig.cd.table$NAME <- as.integer(as.character(orig.cd.table$NAME))

orig.cd.table$CD <- as.character(orig.cd.table$CD)
orig.cd.table$CD <- ifelse(orig.cd.table$CD=="00", 1, orig.cd.table$CD)
orig.cd.table$CD <- as.integer(orig.cd.table$CD)



## Fix the fips codes
fips <- read.csv("./data/fipscodes.csv",
                 header=TRUE
                 )
names(fips) <- c("state", "st.abbr", "fips")
cand.data.dist.fips <- merge(cand.data.dist,
                        fips,
                        by.x="state",
                        by.y="st.abbr",
                        all.x=TRUE,
                        all.y=FALSE
                        )




## Now need to assign to the districts the first word in the CTM terms
## Would prefer to color the word by the winner (D=blue, R=red),
## w/ an alpha = 0.5 or somesuch.

terms.lda.cat <- sapply(1:ncol(terms.lda.district), function(x){
  paste(terms.lda.district[,x], collapse="\n")
})

terms.ctm.cat <- sapply(1:ncol(terms.ctm.district), function(x){
  paste(terms.ctm.district[,x], collapse="\n")
})

terms.topics.lda <- terms.lda.cat[topics.lda.district]
terms.topics.ctm <- terms.ctm.cat[topics.ctm.district]


terms.districts <- data.frame(cand.data.dist.fips$fips,
                              cand.data.dist.fips$district,
                              cand.data.dist.fips$d.victory,
                              terms.topics.lda,
                              terms.topics.ctm
                              )
names(terms.districts) <- c("state", "district",
                            "d.victory","terms.lda",
                            "terms.ctm"
                            )


#terms.districts.ctm <- terms.ctm.district[1:2, topics.ctm.district]
terms.districts$shape <- ifelse(terms.districts$d.victory ==
                                    1,
                                    "D",
                                    "R"
                                    )


## Get the centroids for the districts
cd.centroids <- coordinates(cd.map)

orig.cd.table$centroid.x <- cd.centroids[,1]
orig.cd.table$centroid.y <- cd.centroids[,2]

test <- merge(orig.cd.table,
              terms.districts,
              by.x=c("STATE", "CD"),
              by.y=c("state", "district"),
              all.x=TRUE,
              all.y=FALSE
              )

plot.centroid <- function(x,y, label, color, shape, limits, title,
                          alpha, brewer.palette){

  df <- data.frame(x, y, label, color, shape)
  
  out <- ggplot(na.omit(df),
                aes(x=x,
                    y=y,
                    label=label,
                    color=color,
                    shape=shape
                    )
                ) +
                                        #geom_text(alpha=0.6, size=2) +
                  geom_point(alpha=alpha) +
                    scale_colour_manual(name="District victor",
                values=c("blue", "red")) + 
                      scale_shape(name="Topic terms") +
                        scale_x_continuous(name="", limits=limits) + 
                          scale_y_continuous(name="") + 
                            theme_bw() +
                              opts(title=title)

  return(out)

}

plot.centroid.lda <- plot.centroid(x=test$centroid.x,
                                   y=test$centroid.y,
                                   label=test$terms.lda,
                                   color=test$shape,
                                   shape=test$terms.lda,
                                   alpha=0.7,
                                   title="LDA topics by district",
                                   brewer.palette="Set1",
                                   limits=c(-130, -65)
                                   )

plot.centroid.ctm <- plot.centroid(x=test$centroid.x,
                                   y=test$centroid.y,
                                   label=test$terms.ctm,
                                   color=test$shape,
                                   shape=test$terms.ctm,
                                   alpha=0.7,
                                   title="CTM topics by district",
                                   brewer.palette="Set1",
                                   limits=c(-130, -65)
                                   )


plot.centroid.lda.ne <- plot.centroid(x=test$centroid.x,
                                      y=test$centroid.y,
                                      label=test$terms.lda,
                                      color=test$shape,
                                      shape=test$terms.lda,
                                      alpha=0.7,
                                      title="LDA topics by district\n Northeast zoom",
                                      brewer.palette="Set1",
                                      limits=c(-95, -65)
                                      )

plot.centroid.ctm.ne <- plot.centroid(x=test$centroid.x,
                                      y=test$centroid.y,
                                      label=test$terms.ctm,
                                      color=test$shape,
                                      shape=test$terms.ctm,
                                      alpha=0.7,
                                      title="CTM topics by district\n Northeast zoom",
                                      brewer.palette="Set1",
                                      limits=c(-95, -65)
                                      )

pdf("./plots/ggplot_text_centroid_lda.pdf")
print(plot.centroid.lda)
dev.off()


pdf("./plots/ggplot_text_centroid_ne_lda.pdf")
print(plot.centroid.lda.ne)
dev.off()


pdf("./plots/ggplot_text_centroid_ctm.pdf")
print(plot.centroid.ctm)
dev.off()


pdf("./plots/ggplot_text_centroid_ne_ctm.pdf")
print(plot.centroid.ctm.ne)
dev.off()


## Then plot. Might just be able to plot this as an x-y coord frame.
## Otherwise, could use ggplot to plot the xy coords and transform.
## Though the coords don't really transform well in the geom_map() function.
