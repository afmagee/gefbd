library(coda)
library(parallel)

source("empirical_analysis/src/rank_based_convergence_diagnostics.R")

# We ran 250 simulations to get 200 that passed convergence
max.sims <- 250

# Get arguments
args = commandArgs(trailingOnly=TRUE)

# For parallel
n.cores <- as.numeric(args[1])

# Point to high-level directory, let looping handle the subsets of the simulation study

parents <- c("simulation_study/1_posterior_false_positives",
             "simulation_study/2_posterior_power")

prefix <- "HSMRFBDP_ME_prior_0.5_"

for (parent in parents) {
  # Start reading file names
  all.files <- list.files(paste0(parent,"/output/"),full.names=TRUE)

  # Logs only
  all.logs <- all.files[grepl(".log",all.files,fixed=TRUE)]

  # Grab a list of all simulations that passed convergence
  todo <- c()
  for (i in 1:max.sims) {
    matches_index <- grepl(paste0(prefix,i),all.logs)
    if ( sum(matches_index) > 1 ) {
      todo <- c(todo,i)
    }
  }

  # The number of simulations that passed convergence
  n.sims <- length(todo)

  # Create directory for summaries
  if ( !dir.exists(paste0(parent,"/summaries")) ) {
    dir.create(paste0(parent,"/summaries"))
  }

  res <- mclapply(todo,function(i){
    # Read logs
    this.chain.logs <- all.logs[grepl(paste0("_",i,"_run_"),all.logs)]
    this.chain.logs <- lapply(this.chain.logs,function(f){
      try(read.table(f,sep="\t",header=TRUE,row.names=1,stringsAsFactors=FALSE))
    })
    ngen <- unlist(lapply(this.chain.logs,function(x){dim(x)[1]}))
    if ( any(unlist(lapply(this.chain.logs,class)) == "try-error") || length(unique(ngen)) != 1 ) {
      est.q.025 <- NA
      est.q.975 <- NA
      est.q.05 <- NA
      est.q.95 <- NA
      est.mean <- NA
      est.median <- NA
      est.p0 <- NA
      rank.psrf <- NA
      ess <- NA
    } else {
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
    return(list(index=i,
                est.q.025=est.q.025,
                est.q.975=est.q.975,
                est.q.05=est.q.05,
                est.q.95=est.q.95,
                est.mean=est.mean,
                est.median=est.median,
                est.p0=est.p0,
                rank.psrf=rank.psrf,
                ess=ess
    ))
  },mc.cores=n.cores,mc.preschedule=FALSE)

  # Empty lists (will be made into tables later)
  est.q.025 <- vector("list",n.sims)
  est.q.975 <- vector("list",n.sims)
  est.q.05 <- vector("list",n.sims)
  est.q.95 <- vector("list",n.sims)
  est.mean <- vector("list",n.sims)
  est.median <- vector("list",n.sims)
  est.p0 <- vector("list",n.sims)
  rank.psrf <- vector("list",n.sims)
  ess <- vector("list",n.sims)

  indices <- numeric(n.sims)
  for (i in 1:n.sims) {
    indices[i] <- res[[i]]$index
    est.q.025[[i]] <-  res[[i]]$est.q.025
    est.q.975[[i]] <-  res[[i]]$est.q.975
    est.q.05[[i]] <-  res[[i]]$est.q.05
    est.q.95[[i]] <-  res[[i]]$est.q.95
    est.mean[[i]] <-  res[[i]]$est.mean
    est.median[[i]] <-  res[[i]]$est.median
    est.p0[[i]] <-  res[[i]]$est.p0
    rank.psrf[[i]] <-  res[[i]]$rank.psrf
    ess[[i]] <-  res[[i]]$ess
  }

  est.q.025 <- do.call(rbind,est.q.025)
  est.q.975 <- do.call(rbind,est.q.975)
  est.q.05 <- do.call(rbind,est.q.05)
  est.q.95 <- do.call(rbind,est.q.95)
  est.mean <- do.call(rbind,est.mean)
  est.median <- do.call(rbind,est.median)
  est.p0 <- do.call(rbind,est.p0)
  rank.psrf <- do.call(rbind,rank.psrf)
  ess <- do.call(rbind,ess)

  row.names(est.q.025) <- indices
  row.names(est.q.975) <- indices
  row.names(est.q.05) <- indices
  row.names(est.q.95) <- indices
  row.names(est.mean) <- indices
  row.names(est.median) <- indices
  row.names(rank.psrf) <- indices
  row.names(est.p0) <- indices
  row.names(ess) <- indices

  write.csv(est.q.025,file=paste0(parent,"/summaries/quantile_025.csv"))
  write.csv(est.q.975,file=paste0(parent,"/summaries/quantile_975.csv"))
  write.csv(est.q.05,file=paste0(parent,"/summaries/quantile_05.csv"))
  write.csv(est.q.95,file=paste0(parent,"/summaries/quantile_95.csv"))
  write.csv(est.mean,file=paste0(parent,"/summaries/mean.csv"))
  write.csv(est.median,file=paste0(parent,"/summaries/median.csv"))
  write.csv(est.p0,file=paste0(parent,"/summaries/probability_param_is_0.csv"))
  write.csv(rank.psrf,file=paste0(parent,"/summaries/rank_psrf.csv"))
  write.csv(ess,file=paste0(parent,"/summaries/ess.csv"))

  rm(res)
}
