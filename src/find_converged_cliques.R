suppressMessages(library(coda,warn.conflicts=FALSE,quietly=TRUE))
suppressMessages(library(igraph,warn.conflicts=FALSE,quietly=TRUE))
source("src/rank_based_convergence_diagnostics.R")
source("src/find_converged_cliques_utils.R")

# This is designed to be called with Rscript with several arguments
# arg1: path to and prefix name of logfiles (e.g. ~/folder/output)
# arg2: prefix name of logfiles (e.g. calsoy_as_gonio)
# arg3: mass extinction prior
# arg4: maxPSRF threshold for declaring a group of runs converged


# Get arguments
args = commandArgs(trailingOnly=TRUE)

if (!length(args) == 4) {
  stop("This script requires 4 arguments")
}

OUTPUT_DIR       = args[1]
DATASET          = args[2]
ME_PRIOR         = args[3]
PSRF_THRESHOLD   = as.numeric(args[4])


recycling <- file.path(OUTPUT_DIR,"_unconverged")
if ( !dir.exists(recycling) ) {
  dir.create(recycling)
}

# Get all log files matching prefix (accounts for replicate runs)
# This will not work on windows
rb_logs <- list.files(OUTPUT_DIR,full.names=TRUE)
rb_logs <- rb_logs[grepl(DATASET,rb_logs) & grepl(paste0("prior_",ME_PRIOR,"_"),rb_logs)]

cat("Found",length(rb_logs),"logfiles to analyze!\n")
rb <- lapply(rb_logs,read.table,stringsAsFactors=FALSE,header=TRUE,row.names=1)

# Check for identical runs (seed issues)
rb_means <- do.call(rbind,lapply(rb,colMeans))
run_to_run_dists <- as.matrix(dist(rb_means))
diag(run_to_run_dists) <- -1

has_identical <- any(run_to_run_dists == 0)

if ( has_identical ) {
  to_remove <- which(run_to_run_dists == 0, arr.ind=TRUE)[1,1]

  cat("Found identical logs, removing",rb_logs[to_remove],"\n")

  newpath <- file.path(recycling,basename(rb_logs[to_remove]))
  system2("mv",c(rb_logs[to_remove],newpath))

  rb_logs <- rb_logs[-to_remove]
  rb <- rb[-to_remove]

  run_to_run_dists <- as.matrix(dist(rb_means))
  diag(run_to_run_dists) <- -1

  has_identical <- any(run_to_run_dists == 0)
}

converged <- c()
ngen <- unlist(lapply(rb,function(logfile){
  dim(logfile)[1]
}))
if ( length(unique(ngen)) != 1 ) {
  cat("Logfiles are of different length and are being considered unconverged\n")
} else {
  converged <- findConvergedSet(rb,args[2])
  cat("The converged set includes",length(converged),"logs\n")
}


# Shove unconverged logs into the bin
if ( length(converged) < length(rb) ) {
  unconverged <- (1:length(rb))[!(1:length(rb) %in% converged)]
  for (i in unconverged) {
    newpath <- file.path(recycling,basename(rb_logs[i]))
    system2("mv",c(rb_logs[i],newpath))
  }
}
