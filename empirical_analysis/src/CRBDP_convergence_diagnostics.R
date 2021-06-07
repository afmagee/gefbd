source("src/rank_based_convergence_diagnostics.R")
library(coda)

# Get names of datasets
ds.names <- list.files("data/")
ds.names <- ds.names[grepl(".tre",ds.names,fixed=TRUE)]

# Read in CRBDP analyses
all.analyses <- list.files("output/",full.names=TRUE)
cr.analyses <- all.analyses[grepl("CRBDP",all.analyses)]

# Convergence diagnostics
rpsrf <- vector("list",length(ds.names))
ress <- vector("list",length(ds.names))

for (i in 1:length(ds.names)) {
  ds <- ds.names[i]

  log.files <- cr.analyses[grepl(ds,cr.analyses)]
  posteriors <- lapply(log.files,read.table,sep="\t",header=TRUE,stringsAsFactors=FALSE,row.names=1)

  # Rank-based PSRF
  rpsrf[[i]] <- diagnoseConvergence(posteriors,FALSE)

  # Rank-based ESS
  ress[[i]] <- rankESS(posteriors)
}

# Put into a table
rpsrf <- do.call(cbind,rpsrf)
ress <- do.call(cbind,ress)

colnames(rpsrf) <- ds.names
colnames(ress) <- ds.names

write.csv(rpsrf,"convergence/CRBDP_rank_PSRF.csv",quote=FALSE)
write.csv(ress,"convergence/CRBDP_rank_ESS.csv",quote=FALSE)
