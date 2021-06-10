findConvergedSet <- function(x,maxPSRF) {
  # recover()
  n_runs <- length(x)
  cmat <- matrix(1,n_runs,n_runs)
  for (i in 1:(n_runs-1)) {
    for (j in (i+1):n_runs) {
      # rank-based convergence diagnostics for pair i,j of runs
      psrf <- diagnoseConvergence(x[c(i,j)],FALSE)
      # Did this pair of runs converge?
      cmat[i,j] <- max(apply(psrf,2,max,na.rm=TRUE)) < maxPSRF
      cmat[j,i] <- cmat[i,j]
    }
  }
  components <- components(graph_from_adjacency_matrix(cmat))
  non_singletons <- which(components$csize > 1)

  if ( length(non_singletons) == 0) {
    return(integer(0))
  }

  converged_sets <- vector("list",length(non_singletons))
  idx <- 0
  for (i in non_singletons) {
    idx <- idx + 1
    in_set <- which(components$membership == i)
    together <- findConvergedSubcomponent(x[in_set],cmat[in_set,in_set],maxPSRF)
    converged_sets[[idx]] <- in_set[together]
  }

  # If the full set is converged we're done
  if ( any(lengths(converged_sets) == length(x)) ) {
    return(1:length(x))
  # Otherwise we have to find the best subgrouping that has converged
  } else {
    if ( length(converged_sets) == 1 ) {
      return(converged_sets[[idx]])
    } else {
      mean_lnPP <- unlist(lapply(converged_sets,function(this_set){
        concatenated <- do.call(rbind,x[this_set])
        mean(concatenated$Posterior)
      }))
      return(converged_sets[[which.max(mean_lnPP)]])
    }
  }

}

findConvergedSubcomponent <- function(x,cmat,maxPSRF) {
  # Check if we can just lump them together
  psrf <- diagnoseConvergence(x,FALSE)
  if ( max(psrf,na.rm=TRUE) < maxPSRF ) {
    return(1:length(x))
  }
  # Fall back to cliques
  cliques <- cliques(graph_from_adjacency_matrix(cmat))
  # No singletons
  cliques <- cliques[lengths(cliques) > 1]
  mean_lnPP <- unlist(lapply(cliques,function(this_set){
    concatenated <- do.call(rbind,rb[this_set])
    mean(concatenated$Posterior)
  }))
  return(cliques[[which.max(mean_lnPP)]])
}
