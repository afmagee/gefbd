library(parallel)

# Get arguments
args = commandArgs(trailingOnly=TRUE)

# For parallel
n.cores <- as.numeric(args[1])


source("simulation_study/src/summary_stat_helper.R")

FULL_TREEFILE_DIR <- "simulation_study/4_diversity_through_time/data"

grid.times <- seq(0,253.5,length.out=1001)

treefiles <- list.files(FULL_TREEFILE_DIR,full.names=TRUE)
treefiles <- treefiles[!grepl("calsoy",treefiles)]
treefiles <- treefiles[grepl(".tre",treefiles,fixed=TRUE)]


cat("working on",length(treefiles),"treefiles","\n")

dtt.per.tree <- function(f){

#  f <- treefiles[i]
  phy <- read.tree(f)
  ltt <- tess.get.ltt(phy)
  of <- gsub(".tre",".diversity_through_time.txt",f,fixed=TRUE)

  dtt <- dtt.from.ltt(ltt,grid=grid.times)

  cat(dtt$y,file=of,sep="\n")

}


res <- mclapply(X=treefiles,FUN=dtt.per.tree,mc.cores=n.cores)
