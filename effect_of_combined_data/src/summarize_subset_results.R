library(coda)
library(parallel)

source("empirical_analysis/src/rank_based_convergence_diagnostics.R")
source("empirical_analysis/src/plot_helper.R")

# Get arguments
args = commandArgs(trailingOnly=TRUE)

# For parallel
n.cores <- as.numeric(args[1])

FIGURES_DIR = "effect_of_combined_data/figures"
SUMMARIES_DIR = "effect_of_combined_data/summaries"

## Create figures directory
dir.create(FIGURES_DIR, showWarnings=FALSE)
dir.create(SUMMARIES_DIR, showWarnings=FALSE)

grid.times <- computeGridEndTimes(100,243.5)

# Get names of datasets
ds.names <- c("calsoy_as_gonio.equal.extant.tre","calsoy_as_gonio.equal.extinct.tre")
ds.names <- ds.names[!grepl("priors",ds.names,fixed=TRUE)]
ds.names <- ds.names[grepl(".tre",ds.names,fixed=TRUE)]

all.logs <- list.files("effect_of_combined_data/output/",full.names=TRUE)
all.logs <- all.logs[grepl("HSMRF",all.logs)]


todo <- paste0("effect_of_combined_data/output/HSMRFBDP_ME_prior_0.5_",ds.names)

cat("About to attempt to read logfiles for these analyses:\n")
cat(todo,sep="\n")

res <- mclapply(todo,function(this_ds){
  # Read logs
  this.chain.logs <- all.logs[grepl(basename(this_ds),all.logs)]
  this.chain.logs <- lapply(this.chain.logs,function(f){
    try(read.table(f,sep="\t",header=TRUE,row.names=1,stringsAsFactors=FALSE))
  })
  ngen <- unlist(lapply(this.chain.logs,function(x){dim(x)[1]}))
  est.q.025 <- NA
  est.q.975 <- NA
  est.q.05 <- NA
  est.q.95 <- NA
  est.mean <- NA
  est.median <- NA
  est.p0 <- NA
  rank.psrf <- NA
  ess <- NA
  ds_name <- basename(this_ds)
  
  if ( !any(unlist(lapply(this.chain.logs,class)) == "try-error") && length(unique(ngen)) == 1 ) {
    # Concatenate
    this.chain.log <- do.call(rbind,this.chain.logs)
    # Grab estimates
    est.q.025 <- apply(this.chain.log,2,quantile,probs=0.025)
    est.q.975 <- apply(this.chain.log,2,quantile,probs=0.975)
    est.q.05 <- apply(this.chain.log,2,quantile,probs=0.05)
    est.q.95 <- apply(this.chain.log,2,quantile,probs=0.95)
    est.mean <- apply(this.chain.log,2,mean)
    est.median <- apply(this.chain.log,2,median)
    est.p0 <- apply(this.chain.log,2,function(x){
      sum(x == 0.0)/length(x)
    })
    rank.psrf <- diagnoseConvergence(chains=this.chain.logs,return.both=FALSE)[,1]
    ess <- rankESS(list(this.chain.log))
  }
  
  all.summaries <- list(est.q.025=est.q.025,
                        est.q.975=est.q.975,
                        est.q.05=est.q.05,
                        est.q.95=est.q.95,
                        est.mean=est.mean,
                        est.median=est.median,
                        est.p0=est.p0,
                        rank.psrf=rank.psrf,
                        ess=ess)
  all.summaries <- do.call(rbind,all.summaries)
  
  out.file <- paste0(SUMMARIES_DIR,"/",ds_name,"_summary.csv")
  
  tmp <- try(write.csv(all.summaries,file=out.file))
  
  return(paste0(ifelse(file.exists(out.file),"Successful","Failure")," in creating summary file for ",this_ds))
},mc.cores=n.cores,mc.preschedule=FALSE)

cat("###################")

cat(unlist(res),sep="\n")
