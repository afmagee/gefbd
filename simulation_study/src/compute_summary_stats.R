time <- system.time({

source("simulation_study/src/summary_stat_helper.R")

dirs <- c("simulation_study/1_posterior_false_positives/data/",
          "simulation_study/2_posterior_power/data/",
          "simulation_study/3_treatment/data/")

for (dir in dirs) {

  treefiles <- list.files(dir,full.names=TRUE)
  treefiles <- treefiles[!grepl("calsoy",treefiles)]
  treefiles <- treefiles[grepl(".tre",treefiles,fixed=TRUE)]
  cat(dir,"\n")
  cat("working on",length(treefiles),"treefiles","\n")
  pb <- txtProgressBar(min=0,max=length(treefiles),style=3)
  for (i in 1:length(treefiles)) {
    f <- treefiles[i]
    phy <- read.tree(f)
    stats <- tess.compute.summary.stats(phy)
    of <- gsub(".tre",".stats.txt",f,fixed=TRUE)
    cat(names(stats),sep=",",file=of)
    cat("\n",file=of,append=TRUE)
    cat(stats,sep=",",file=of,append=TRUE)
    cat("\n",file=of,append=TRUE)
    setTxtProgressBar(pb,i)
  }
  cat("\n")
}

})

print(time)