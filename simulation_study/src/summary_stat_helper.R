library(ape)

# From most current version of TESS on GitHub
tess.branching.times <- function(phy, tip.age.threshold=1e-5) {
  
  # Do it recursively  
  fx <- function(phy, node, cur_time, nodes) {
    
    children <- phy$edge[,2][phy$edge[,1] == node]
    
    for (i in 1:length(children)) {
      child <- children[i]
      child_edge <- which(phy$edge[,2] == child)
      branch_time <- cur_time + phy$edge.length[child_edge]
      
      nodes$age[child]              <- branch_time
      nodes$age_parent[child]       <- nodes$age[node]
      nodes$sampled_ancestor[child] <- ( length(phy$edge[,2][phy$edge[,1] == child]) == 1 )
      nodes$tip[child]              <- ( length(phy$edge[,2][phy$edge[,1] == child]) == 0 )
      
      if (child > length(phy$tip.label)) {
        nodes <- fx(phy, node = child, cur_time = branch_time, nodes)  
      }
      
    }
    
    return (nodes)
  }
  
  nodes <- list(sampled_ancestor=c(),fossil_tip=c(),age_parent=c(),age=c(),tip=c())
  
  # do the recursive call starting with the root
  nodes$age[length(phy$tip.label) + 1]              <- 0.0
  nodes$age_parent[length(phy$tip.label) + 1]       <- Inf
  nodes$tip[length(phy$tip.label) + 1]              <- FALSE
  nodes$sampled_ancestor[length(phy$tip.label) + 1] <- FALSE
  nodes$fossil_tip[length(phy$tip.label) + 1]       <- FALSE
  nodes <- fx(phy, node = length(phy$tip.label) + 1, cur_time = 0, nodes)
  
  max_bt <- max(nodes$age)
  nodes$age        <- max_bt - nodes$age
  nodes$age_parent <- max_bt - nodes$age_parent
  
  nodes$age_parent[length(phy$tip.label) + 1]       <- Inf
  
  nodes$age[ nodes$age < tip.age.threshold ] <- 0.0
  nodes$fossil_tip <- nodes$age > tip.age.threshold & nodes$tip
  nodes$tip <- nodes$fossil_tip == FALSE & nodes$tip
  
  return( nodes )
  
}

# Not currently in TESS, something similar may be in future versions
# MRCA tells us whether the tree starts with one lineage or at the TMRCA
# fossil.tol is a tlerance for determining whether a tip is extant or not
# Statistics computed:
#   gamma == gamma statistic (ignores sampled ancestors)
#   colless == Colless' Imbalance statistic (ignores sampled ancestors)
#   tippyness == ratio of sum of pendant branch lengths to sum of all branch lengths (1 - stemmyness)
#   extant.tippyness == ratio of sum of branch lengths leading to extant tips to sum of all branch lengths (1 - stemmyness)
#   extinct.tippyness == ratio of sum of branch lengths leading to extinct tips to sum of all branch lengths (1 - stemmyness)
#   tl == tree length (sum of all branch lengths)
#   th == tree height (depth of root from youngest sample)
#   wwtv == weighted waiting time variance (measures variability in weighting times accounting for number of taxa per interval, treats all event-sampling as a single time)
#   lb == longest branch (sampled ancestors break up branch lengths)
#   mtml == minimum time at which the maximum number of lineages existed (minimum because if there are multiple intervals with this n we take the most recent and because we measure to the youngest end of the interval, will be 0 for trees without serial samples)
#   asa == average sample age (average age of all tips, fossil tips, and sampled ancestors)
#   msa == maximum sample age (age of the oldest fossil/sampled ancestor in the tree)
#   psb == proportion of sample ages below youngest branching time (more recent than the most recent branching time)
#   rmom == method-of-moments estimator of net diversification rate
#   ntips == number of (fossil and/or at-present) tips
#   nsa == number of sampled ancestors
#   pes == proportion of extant samples (among all samples)
tess.compute.summary.stats <- function(x,stats="all",MRCA=TRUE,fossil.tol=1e-5,...) {
  is.tree <- FALSE
  bt <- x
  if ( inherits(x,"phylo") ) {
    is.tree <- TRUE
    bt <- tess.branching.times(x,tip.age.threshold=fossil.tol,...)
  }
  tree_stats <- c("tippyness","extant.tippyness","extinct.tippyness","tl","colless","lb")
  bt_stats <- c("gamma","th","wwtv","tml","asa","msa","psb","rmom","ntips","nsa","mtml","pes")
  if ( tolower(stats) == "all" ) {
    stats <- c(tree_stats,bt_stats)
  } else {
    stats <- tolower(stats)
    if ( any(stats %in% tree_stats) && !is.tree ) {
      stop("To compute tree-based statistics, input must be tree")
    }
  }
  
  res <- list()
  # recover()
  
  # tree-based stats
  if ( "colless" %in% stats ) {
    x_bifurcating <- x
    if ( has.singles(x) ) {
      x_bifurcating <- collapse.singles(x)
    }
    # number of children
    nd <- node.depth(x_bifurcating)
    ntax <- length(x$tip.label)
    summand <- sapply((ntax+1):(ntax+x_bifurcating$Nnode),function(node){
      children <- x_bifurcating$edge[x_bifurcating$edge[,1] == node,2]
      max(nd[children]) - min(nd[children])
    })
    res$colless <- sum(summand)/((ntax-1)*(ntax-2)/2)
  }
  if ( "tl" %in% stats ) {
    res$tl <- sum(x$edge.length)
  }
  if ( "tippyness" %in% stats ) {
    tl <- sum(x$edge.length)
    ttl <- sum(x$edge.length[!(x$edge[,2] %in% x$edge[,1])])
    res$tippyness <- ttl/tl
  }
  if ( "extant.tippyness" %in% stats ) {
    y <- drop.fossil(x)
    tl <- sum(x$edge.length)
    ttl <- sum(y$edge.length[!(y$edge[,2] %in% y$edge[,1])])
    res$extant.tippyness <- ttl/tl
  }
  if ( "extinct.tippyness" %in% stats ) {
    y <- drop.fossil(x)
    z <- drop.tip(x,tip=y$tip.label)
    tl <- sum(x$edge.length)
    ttl <- sum(z$edge.length[!(z$edge[,2] %in% z$edge[,1])])
    res$extinct.tippyness <- ttl/tl
  }
  if ( "stemmyness" %in% stats ) {
    tl <- sum(x$edge.length)
    ttl <- sum(x$edge.length[!(x$edge[,2] %in% x$edge[,1])])
    res$stemmyness <- 1 - ttl/tl
  }
  if ( "lb" %in% stats ) {
    res$lb <- max(x$edge.length)
  }
  
  # branching-time based stats
  
  # convenient object representation of branching times
  # column 1 is age of node
  # column 2 tells us whether this node adds/subtracts a lineage (or not if sampled ancestor)
  # column 3 is number of lineages alive in interval
  ages <- matrix(NA,nrow=length(bt$age),ncol=3)
  ages[,1] <- bt$age 
  ages[,2] <- 1 
  ages[bt$tip,2] <- -1
  ages[bt$fossil_tip,2] <- -1
  ages[bt$sampled_ancestor,2] <- 0
  ages <- ages[order(ages[,1],decreasing=TRUE),]
  ages[,3] <- cumsum(ages[,2]) + MRCA
  
  nevents <- dim(ages)[1]
  
  epsilon <- 1e-8
  
  if ( "gamma" %in% stats ) {
    n <- ages[-dim(ages)[1],3]
    delta_t <- ages[-dim(ages)[1],1] - ages[-1,1]
    
    n <- n[delta_t > 0]
    delta_t <- delta_t[delta_t > 0]
    
    big_t <- sum(n*delta_t)
    
    n <- n[-length(n)]
    delta_t <- delta_t[-length(delta_t)]
    
    res$gamma <- (1/(length(n)) * sum(cumsum(n*delta_t)) - (big_t/2))/(big_t * sqrt(1/(12 * length(n))))
  }
  if ( "th" %in% stats ) {
    res$th <- max(bt$age)
  }
  if ( "wwtv" %in% stats ) {
    n <- ages[-nevents,3]
    delta_t <- ages[-nevents,1] - ages[-1,1]
    res$wwtv <- var(n*delta_t)
  }
  if ( "mtml" %in% stats ) {
    tmp <- ages[ages[,2] != 0,]
    has_max <- min(which(tmp[,3] == max(tmp[,3])))
    res$mtml <- tmp[has_max+1,1]
  }
  if ( "asa" %in% stats ) {
    res$asa <- mean(ages[ages[,2] < 1,1])
  }
  if ( "msa" %in% stats ) {
    res$msa <- max(ages[ages[,2] < 1,1])
  }
  if ( "psb" %in% stats ) {
    youngest_bt <- min(ages[ages[,2] == 1,1])
    res$psb <- sum(ages[ages[,2] < 1,1] < youngest_bt)/sum(ages[,2] < 1)
  }
  if ( "rmom" %in% stats ) {
    n_births <- sum(ages[,2] == 1) - MRCA
    age <- max(ages[,1])
    r_hat <- log(n_births/(1 + MRCA))/age
    res$rmom <- r_hat
  }
  if ( "ntips" %in% stats ) {
    res$ntips <- sum(bt$fossil_tip | bt$tip)
  }
  if ( "nsa" %in% stats ) {
    res$nsa <- sum(bt$sampled_ancestor)
  }
  if ( "pes" %in% stats ) {
    res$pes <- sum(bt$tip)/(sum(bt$tip) + sum(bt$fossil_tip) + sum(bt$sampled_ancestor))
  }
  
  return(unlist(res)) 
}


# Not currently in TESS, something similar may be in future versions
# MRCA tells us whether the tree starts with one lineage or at the TMRCA
# fossil.tol is a tlerance for determining whether a tip is extant or not
# returs list, with x coords the times and y coords the numbers of lineages
tess.get.ltt <- function(x,fossil.tol=1e-5,MRCA=TRUE,...) {
  # recover()
  
  is.tree <- FALSE
  bt <- x
  if ( inherits(x,"phylo") ) {
    is.tree <- TRUE
    bt <- tess.branching.times(x,tip.age.threshold=fossil.tol,...)
  }

  # convenient object representation of branching times
  # column 1 is age of node
  # column 2 tells us whether this node adds/subtracts a lineage (or not if sampled ancestor)
  # column 3 is number of lineages alive in interval
  ages <- matrix(NA,nrow=length(bt$age),ncol=3)
  ages[,1] <- bt$age 
  ages[,2] <- 1 
  ages[bt$tip,2] <- -1
  ages[bt$fossil_tip,2] <- -1
  ages[bt$sampled_ancestor,2] <- 0
  ages <- ages[order(ages[,1],decreasing=TRUE),]
  ages[,3] <- cumsum(ages[,2]) + MRCA
  
  # we don't want the LTT to go to 0 at the present due to samples at the present
  ages <- ages[ages[,1] > fossil.tol,]
  
  times <- c()
  ntax <- c()
  
  for (i in 1:dim(ages)[1]) {
    times <- c(times,rep(ages[i,1],2))
    ntax <- c(ntax,rep(ages[i,3],2))
  }

  # times <- rep(ages[,1],2)
  # ntax <- rep(ages[,3],2)
  # 
  # key <- order(times,decreasing=TRUE)
  # 
  # times <- times[key]
  # ntax <- ntax[key]

  times <- times[-1]
  times <- c(times,0)
  
  return(list(x=times,y=ntax))
}

# Gets average taxonomic diversity in nbins windows over time from ltt curve
# If given, average is over the grid in grid, otherwise a grid with nbins evenly spaced bins are used
dtt.from.ltt <- function(ltt,grid,nbins=NA) {
  
  # recover()
  
  EPS <- 1e-10
  
  bin_widths <- c()
  if ( length(grid) > 3 & all(is.numeric(grid)) ) {
    nbins <- length(grid) - 1
    grid <- sort(grid,decreasing=FALSE)
    if ( !grid[1] == 0 && grid[nbins+1] == max(ltt$x) ) {
      stop("Invalid grid")
    }
    bin_widths <- grid[-1] - grid[-(nbins+1)]
  } else {
    grid <- seq(min(ltt$x),max(ltt$x),length.out=nbins+1)
    bin_widths <- rep((max(ltt$x) - min(ltt$x))/nbins,nbins)
  }
  
  tree_age <- max(ltt$x)
  
  # It's easier to work fully in backwards time with just the times and the numbers of taxa at that time
  t <- rev(ltt$x[seq(1,length(ltt$x)-1,2)])
  n <- rev(ltt$y[seq(1,length(ltt$y)-1,2)])
  
  avg_per_interval <- numeric(nbins)
  props <- numeric(nbins)
  t_idx <- 1
  t_at <- 0
  n_avg <- 0
  grid_idx <- 1
  cfrac <- 0
  while (t_at < tree_age ) {
    t_next <- min(t[t_idx],grid[grid_idx+1])
    frac <- (t_next - t_at)/bin_widths[grid_idx]
    n_avg <- n_avg + frac * n[t_idx]
    
    t_at <- t_next
    
    # If we cross into a new grid cell, reset trackers
    if ( (t_at < grid[grid_idx+1] + EPS) & (t_at > grid[grid_idx+1] - EPS) ) {
      avg_per_interval[grid_idx] <- n_avg
      n_avg <- 0
      grid_idx <- grid_idx + 1
    } 
    
    # If we cross into a new ltt interval, change index
    if ( (t_at < t[t_idx] + EPS) & (t_at > t[t_idx] - EPS) ) {
      t_idx <- t_idx + 1
    }
  }
  
  ntax <- as.numeric(t(matrix(rep(avg_per_interval,2),ncol=2,nrow=nbins)))
  times <- as.numeric(t(matrix(rep(grid[2:nbins],2),ncol=2,nrow=nbins-1)))
  times <- c(grid[1],times,grid[nbins+1])
  
  return(list(x=times,y=ntax))
}
