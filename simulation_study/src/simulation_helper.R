source("empirical_analysis/src/plot_helper.R")

library(TESS)

# Gets continuous parameters and mass extinction parameters from Rev posterior log
# If mass.extinction.name == NA, there are no Mass Extinctions (all ME probabilities are 0)
getParameters <- function(rb.posterior,speciation.name,extinction.name,sampling.name,mass.extinction.name=NA) {

  parameter_names <- c(speciation.name,extinction.name,sampling.name)

  parameters <- vector("list",3)

  # recover()
  for (i in 1:3) {
    key <- paste0("^",parameter_names[i])
    par <- rb.posterior[,grepl(key,names(rb.posterior))]
    par <- par[,grepl("[0-9]",names(par))]
    parameters[[i]] <- par
  }

  if ( is.character(mass.extinction.name) ) {
    parameter_names <- c(parameter_names,mass.extinction.name)
    key <- paste0("^",parameter_names[4])
    par <- rb.posterior[,grepl(key,names(rb.posterior))]
    par <- par[,grepl("[0-9]",names(par))]
    parameters[[4]] <- par
  } else {
    parameter_names <- c(parameter_names,"mass_extinction_probabilities")
    parameters[[4]] <- matrix(0,nrow=dim(parameters[[1]])[1],ncol=dim(parameters[[1]])[2]-1)
  }

  names(parameters) <- parameter_names

  return(parameters)

}

getSimulatorInput <- function(params,index,guarantee.ME.at.index=NA,ME.prob=NA,ME.minimum.prob=0.5) {
  # recover()

  n <- dim(params[[1]])[2]

  if ( is.finite(guarantee.ME.at.index) && ME.prob > 0 && ME.prob < 1 && params[[4]][index,guarantee.ME.at.index] < ME.minimum.prob ){
    params[[4]][index,guarantee.ME.at.index] <- ME.prob
  }

  spn <- paste0("speciation_rate[",1:n,"][1] <- ",params[[1]][index,])
  exn <- paste0("extinction_rate[",1:n,"][1] <- ",params[[2]][index,])
  fos <- paste0("fossilization_rate[",1:n,"][1] <- ",params[[3]][index,])
  mep <- paste0("mass_extinction_probabilities[",1:n,"][1] <- Probability(",c(0,params[[4]][index,]),")")

  return(c(spn,exn,fos,mep))
}

# expectedTreeSize <- function(index,parameters,timeline,root.age) {
#   recover()
#
#   lambda <- rev(parameters[[1]][index,])
#   mu     <- rev(parameters[[2]][index,])
#   phi    <- rev(parameters[[3]][index,])
#   MU     <- 1 - rev(parameters[[4]][index,])
#
#   if ( !(all(timeline == sort(timeline))) ) {
#     stop("Improper sorted timeline")
#   }
#
#   if ( length(timeline) != (length(lambda) - 1) ) {
#     stop("Timeline not correct length")
#   }
#
#   # flip timeline to forward time
#   timeline <- c(timeline,root.age)
#   timeline <- rev(root.age - timeline)
#
#   # forward time functions
#   lambda_t <- function(times) {
#     sapply(times,function(t_){
#       # lambda[findInterval(t_,timeline)]
#       lambda[max(which(timeline <= t_))]
#     })
#   }
#
#   mu_t <- function(times) {
#     sapply(times,function(t_){
#       # mu[findIntervalt_,timeline)]
#       mu[max(which(timeline <= t_))]
#     })
#   }
#
#   phi_t <- function(times) {
#     sapply(times,function(t_){
#       phi[findInterval(t_,timeline)]
#     })
#   }
#
#   # forward time function that is the rate of accumulation of taxon samples
#   dN_dt <- function(times) {
#     sapply(times,function(t_){
#       phi_t(t_) * TESS::tess.nTaxa.expected(begin=0,t=t_,end=t_,lambda=lambda_t,mu=mu_t)#,massExtinctionTimes=timeline[-1],massExtinctionSurvivalProbabilities=MU,MRCA=TRUE,reconstructed=FALSE)
#     })
#   }
#
#   integrate(dN_dt,0,root.age)
#
#
# }

# expectedTreeSize <- function(index,parameters,timeline,root.age,n.time.slices=1000) {
#   # recover()
#
#   lambda <- rev(parameters[[1]][index,])
#   mu     <- rev(parameters[[2]][index,])
#   phi    <- rev(parameters[[3]][index,])
#   MU     <- 1 - rev(parameters[[4]][index,])
#
#   if ( !(all(timeline == sort(timeline))) ) {
#     stop("Improper sorted timeline")
#   }
#
#   if ( length(timeline) != (length(lambda) - 1) ) {
#     stop("Timeline not correct length")
#   }
#
#   # flip timeline to forward time
#   timeline <- c(timeline,root.age)
#   timeline <- rev(root.age - timeline)
#
#   # forward time functions
#   lambda_t <- function(t_) {
#     lambda[findInterval(t_,timeline,left.open=FALSE)]
#   }
#
#   mu_t <- function(t_) {
#     mu[findInterval(t_,timeline,left.open=FALSE)]
#   }
#
#   phi_t <- function(t_) {
#     phi[findInterval(t_,timeline,left.open=FALSE)]
#   }
#
#   dt <- root.age/n.time.slices
#
#   ME_to_to <- 1
#
#   t_ <- 0
#   ntaxa <- 2
#   nsamps <- 0
#
#   # N_t <- c()
#
#   while (t_ < root.age) {
#     # N_t <- c(N_t,ntaxa)
#     nsamps <- nsamps + ntaxa * phi_t(t_) * dt
#     ntaxa <- ntaxa + ntaxa * (lambda_t(t_) - mu_t(t_)) * dt
#     t_ <- t_ + dt
#
#     if ( ME_to_to <= length(MU) && t_ > timeline[1+ME_to_to] ) {
#       ntaxa <- ntaxa * MU[ME_to_to]
#       ME_to_to <- ME_to_to + 1
#     }
#   }
#
#   return(nsamps)
# }
