library(coda)
library(parallel)

source("src/rank_based_convergence_diagnostics.R")
source("src/plot_helper.R")

# Get arguments
args = commandArgs(trailingOnly=TRUE)

# For parallel
n.cores <- as.numeric(args[1])

FIGURES_DIR = "empirical_analysis/figures"
SUMMARIES_DIR = "empirical_analysis/summaries"

grid.times <- computeGridEndTimes(100,243.5)

# Get names of datasets
ds.names <- list.files("empirical_analysis/data/")
ds.names <- ds.names[!grepl("priors",ds.names,fixed=TRUE)]
ds.names <- ds.names[grepl(".tre",ds.names,fixed=TRUE)]

############
# Summarize results WITH mass extinctions
############

ME.priors <- c("0","0.1","0.5","1","2","5")

## Create figures directory
dir.create(FIGURES_DIR, showWarnings=FALSE)
dir.create(SUMMARIES_DIR, showWarnings=FALSE)

todo <- c()

for (i in 1:6) {
  this.ME <- ME.priors[i]

  for (ds in ds.names) {

    this_combo <- paste0("empirical_analysis/output/HSMRFBDP_ME_prior_",this.ME,"_",ds,"")

    todo <- c(todo,this_combo)
  }
}


all.logs <- list.files("empirical_analysis/output/",full.names=TRUE)
all.logs <- all.logs[grepl("HSMRF",all.logs)]

cat("About to attempt to read logfiles for these analyses:\n")
cat(todo,sep="\n")

COMPUTE_NET_DIVERSIFICATION <- TRUE

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
    # compute net div (maybe)
    if ( COMPUTE_NET_DIVERSIFICATION ) {
      spn <- this.chain.log[,grepl("^speciation",names(this.chain.log))]
      spn <- spn[,grepl("[0-9]",names(spn))]
      exn <- this.chain.log[,grepl("^extinction",names(this.chain.log))]
      exn <- exn[,grepl("[0-9]",names(exn))]
      nd <- spn - exn
      npar <- dim(this.chain.log)[2]
      names(nd) <- paste0("net_diversification.",1:dim(spn)[2],".")
      this.chain.log <- cbind(this.chain.log,nd)
      # names(this.chain.log)[npar+(1:dim(spn)[2])] <- paste0("net_diversification.",1:dim(spn)[2],".")
    }
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
