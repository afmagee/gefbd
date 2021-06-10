library(RColorBrewer)
library(viridis)
library(geoscale)

computeGridCenterTimes <- function(n,grid.end) {
  start <- 0.5/n * grid.end
  end <- (n-0.5)/n * grid.end
  return(seq(start,end,grid.end/n))
}

computeGridEndTimes <- function(n,grid.end) {
  return(seq(1/n*grid.end,((n-1)/n)*grid.end,grid.end/n))
}

# Converts between breakpoints of an arbitrary grid and center points of grid cells
# If grid is unevenly sized, last time is based on average grid cell size
intervalTimesToCenterTimes <- function(grid) {
  avg_width <- mean(grid[-1] - grid[-length(grid)])
  grid <- c(0,grid)
  times <- (grid[-1] + grid[-length(grid)])/2
  times <- c(times,grid[length(grid)]+avg_width/2)
  return(times)
}

getConcatenatedLogFile <- function(rb.file.prefix) {
  # recover()

  # Get all log files matching prefix (accounts for replicate runs)
  # This will not work on windows
  folder <- dirname(rb.file.prefix)
  rb_logs <- list.files(folder,full.names=TRUE)
  rb_logs <- rb_logs[grepl(basename(rb.file.prefix),rb_logs)]
  rb <- do.call(rbind,lapply(rb_logs,read.table,stringsAsFactors=FALSE,header=TRUE))
  return(rb)

}

plotContinuousParameters <- function(rb.posterior,posterior.summary=NULL,interval.times,parameter.names,ci.width=0.9) {

  # recover()

  # Times for plotting
  # The conversion to grid centers assumes times are positive
  # But in the end we'll want negative times so that things plot past to present
  if (interval.times[1] < 0) {
    interval.times <- -interval.times
  }
  times <- -intervalTimesToCenterTimes(interval.times)

  # De-clutter x-axis
  oldest <- ceiling(max(-times))
#  pretty_times <- round(-seq(0,oldest,length.out=10))
  pretty_times <- -pretty(c(0,oldest))

  colors <- brewer.pal(8,"Dark2")[1:length(parameter.names)]
  transparents <- paste0(colors,"90")

  alpha <- (1 - ci.width)/2

  par(mfrow=c(1,length(parameter.names)),mai=c(0.5,0.5,0.05,0.05),omi=c(0.01,0.01,0.01,0.01))
  for (i in 1:length(parameter.names)) {
    key <- paste0("^",parameter.names[i])
    
    if ( is.null(posterior.summary) ) {
      
      if ( !inherits(rb.posterior,"data.frame") ) {
        stop("Input must be a data.frame")
      }
      
      par <- rb.posterior[,grepl(key,names(rb.posterior))]
      par <- par[,grepl("[0-9]",names(par))]
      
      q.low <- apply(par,2,quantile,prob=alpha)
      q.high <- apply(par,2,quantile,prob=1-alpha)
      est <- apply(par,2,quantile,prob=0.5)
    } else {
      
      if ( !inherits(posterior.summary,"data.frame") ) {
        stop("Input must be a data.frame")
      }
      
      par <- posterior.summary[,grepl(key,names(posterior.summary))]
      par <- par[,grepl("[0-9]",names(par))]
      
      if ( ci.width == 0.95 ) {
        low.id <- "q.025"
        high.id <- "q.975"
      } else if ( ci.width == 0.9 ) {
        low.id <- "q.05"
        high.id <- "q.95"
      } else {
        stop("Must provide full posterior for CIs other than 90% or 95%.")
      }
      
      q.low <- par[grepl(low.id,row.names(posterior.summary)),]
      q.high <- par[grepl(high.id,row.names(posterior.summary)),]
      est <- par[grepl("median",row.names(posterior.summary)),]
    }

    r <- range(c(q.low,q.high))

    plot(NULL,NULL,ylim=r,xlim=range(times),log="",ylab="",xlab="",xaxt="n",cex.axis=1)
    polygon(x=c(times,rev(times)),y=c(q.low,rev(q.high)),col=transparents[i],border=NA)
    lines(times,est,lwd=3,col=colors[i])

    mtext(parameter.names[i],2,line=2.25,cex=1)
    axis(side=1,at=pretty_times,labels=-pretty_times,cex.axis=1)
    mtext("Time before present",1,line=2.25,cex=1)
  }
}

# posterior.summary must be a nested list
# the outer list will be used to plot multiple ROWS in the multi-panel figure
# the inner list will be used to plot multiple LINES within a cell
plotContinuousParametersMulti <- function(posterior.summaries,interval.times,parameter.names,me.prior.expectations,ci.width=0.9,viridis.option="D",plot.legend=TRUE,cex.axis=1,all.y.lim=NA,RHS.txt=NA,geo.timeline=FALSE,geo.abbreviations=TRUE,...) {
  # recover()

  if ( hasArg("omi") ) {
    OMI <- list(...)$omi
  } else {
    OMI <- c(0.5,0.01,0.25,0.05)
  }

  if ( hasArg("mai") ) {
    MAI <- list(...)$mai
  } else {
    MAI <- c(0.075,0.25,0.05,0.075)
  }
  
  # recover()
  
  is_df <- lapply(posterior.summaries,function(x){
    lapply(x,function(y){
      inherits(y,"data.frame")
    })
  })
  
  if ( !all(unlist(is_df)) ) {
    stop("posterior.summaries must be a list of lists of data.frames")
  }
  
  # Times for plotting
  # The conversion to grid centers assumes times are positive
  # But in the end we'll want negative times so that things plot past to present
  if (interval.times[1] < 0) {
    interval.times <- -interval.times
  }
  times <- -intervalTimesToCenterTimes(interval.times)
  
  # De-clutter x-axis
  oldest <- ceiling(max(-times))
  #  pretty_times <- round(-seq(0,oldest,length.out=10))
  pretty_times <- -pretty(c(0,oldest))
  
  colors <- viridis_pal(option=viridis.option)(length(posterior.summaries[[1]]))
  transparents <- viridis_pal(option=viridis.option,alpha=90/256)(length(posterior.summaries[[1]]))

  nrows <- length(posterior.summaries)
  ntrajectories <- length(posterior.summaries[[1]])
  
  # Get common scales for all plots of the same parameters
  r <- vector("list",length(parameter.names))
  for (i in 1:length(parameter.names)) {
    
    r[[i]] <- c(Inf,-Inf)
    
    key <- paste0("^",parameter.names[i])
    
    for (j in 1:nrows) {
      
      for (k in 1:ntrajectories) {
        posterior.summary <- posterior.summaries[[j]][[k]]
        
        par <- posterior.summary[,grepl(key,names(posterior.summary))]
        par <- par[,grepl("[0-9]",names(par))]
        
        if ( ci.width == 0.95 ) {
          low.id <- "q.025"
          high.id <- "q.975"
        } else if ( ci.width == 0.9 ) {
          low.id <- "q.05"
          high.id <- "q.95"
        } else {
          stop("Must provide full posterior for CIs other than 90% or 95%.")
        }
        
        q.low <- as.numeric(par[grepl(low.id,row.names(posterior.summary)),])
        q.high <- as.numeric(par[grepl(high.id,row.names(posterior.summary)),])
        est <- as.numeric(par[grepl("median",row.names(posterior.summary)),])
        
        est <- c(est,q.low,q.high)
        
        r[[i]][1] <- ifelse(min(est) < r[[i]][1],min(est),r[[i]][1])
        r[[i]][2] <- ifelse(max(est) > r[[i]][2],max(est),r[[i]][2])
        
      }
    }
  }
  
  par(mfrow=c(nrows,length(parameter.names)),mai=MAI,omi=OMI,xpd=TRUE)
  for (j in 1:nrows) {
    for (i in 1:length(parameter.names)) {
      key <- paste0("^",parameter.names[i])
      
      est <- vector("list",ntrajectories)
      q.low <- vector("list",ntrajectories)
      q.high <- vector("list",ntrajectories)
      
      for (k in 1:ntrajectories) {
        posterior.summary <- posterior.summaries[[j]][[k]]
        
        par <- posterior.summary[,grepl(key,names(posterior.summary))]
        par <- par[,grepl("[0-9]",names(par))]
        
        q.low[[k]]  <- as.numeric(par[grepl(low.id,row.names(posterior.summary)),])
        q.high[[k]] <- as.numeric(par[grepl(high.id,row.names(posterior.summary)),])
        est[[k]]    <- as.numeric(par[grepl("median",row.names(posterior.summary)),])
        
      }
      
      yl <- r[[i]]
      if ( is.list(all.y.lim) && !is.na(all.y.lim[[i]][1]) ) {
        yl <- all.y.lim[[i]]
      }
      
      plot(NULL,NULL,ylim=yl,xlim=range(times),log="",ylab="",xlab="",xaxt="n",cex.axis=cex.axis)
      
      # Medians only
      for (k in 1:ntrajectories) {
        lines(times,est[[k]],lwd=3,col=colors[k])
      }
      
      for (k in 1:ntrajectories) {
        q_high <- q.high[[k]]
        q_high[q_high > par("usr")[4]] <- par("usr")[4]
        q_low <- q.low[[k]]
        q_low[q_low < par("usr")[3]] <- par("usr")[3]
        polygon(x=c(times,rev(times)),y=c(q_low,rev(q_high)),col=transparents[k],border=NA)
        lines(times,est[[k]],lwd=3,col=colors[k])
      }
      
      if ( i == length(parameter.names) && !is.na(RHS.txt[1]) ) {
        mtext(RHS.txt[j],side=4,line=0.5)
      }
      
      if ( j == 1 ) {
        mtext(parameter.names[i],3,line=1,cex=1)
        
        if ( i == length(parameter.names) && plot.legend ) {
          legend("top",legend=paste0(me.prior.expectations),fill=colors,border=NA,bty="n")
        }
      }
      
      
      if ( j == nrows ) {
        if ( !geo.timeline ) {
          axis(side=1,at=pretty_times,labels=-pretty_times,cex.axis=cex.axis)
          mtext("Time before present (Ma)",1,line=2.25,cex=1)
        } else {
          # Geological timescale wrangling
          timescales <- NULL
          data(timescales,envir=environment())
          timescales <- timescales$ICS2015
          timescales <- timescales[timescales[,"Type"] == "Period",]
          timescales <- timescales[timescales[,"Start"] >= min(interval.times),]
          timescales <- timescales[timescales[,"End"] <= max(interval.times),]
          starts <- -timescales$End
          ends <- -timescales$Start
          if ( ends[length(ends)] < par("usr")[1] ) {
            ends[length(ends)] <- par("usr")[1]
          }
          if ( starts[1] > par("usr")[2] ) {
            starts[1] <- par("usr")[2]
          }
          
          rcols <- rgb(timescales$Col_R,timescales$Col_G,timescales$Col_B,maxColorValue=256)
          
          geo_box_top <-par("usr")[3]
          geo_box_bottom <- par("usr")[3]-0.075*(par("usr")[4]-par("usr")[3])
          rect(rev(ends),geo_box_top,rev(starts),geo_box_bottom,col=rev(rcols),border=NA)
          if ( geo.abbreviations ) {
            geo_names <- as.character(rev(timescales$Abbr))
            geo_names[geo_names %in% c("Q")] <- ""
            geo_names[geo_names == "P"] <- "Pg"
            geo_names[geo_names == "C"] <- "K"
          } else {
            geo_names <- as.character(rev(timescales$Name))
            # can't see these anyways
            geo_names[geo_names %in% c("Neogene","Quat.")] <- ""
            
          }
          
          text(x=(rev(ends)+rev(starts))/2,y=(geo_box_top+geo_box_bottom)/2,labels=geo_names)
          
          # And an axis label
          axis(side=1,at=pretty_times,labels=-pretty_times,cex.axis=cex.axis,pos=geo_box_bottom)
          mtext("Time before present (Ma)",1,line=4.25,cex=1)
          
        }
        box()
      }
      
    }
    
  }

  }



plotMEBF <- function(rb.posterior,posterior.summary=NULL,interval.times,parameter.name,prior.probability,geo.timeline=FALSE,geo.abbreviations=TRUE) {
  # recover()
  par(mai=c(0.75,0.75,0.05,0.05),omi=c(0.01,0.01,0.01,0.01))

  # Get mass extinction probabilities
  if ( is.null(posterior.summary) ) {
    ME <- rb.posterior[,grepl(parameter.name,names(rb.posterior))]
    ME <- ME[,grepl("[0-9]",names(ME))] #ME is a vector parameter, if it doesn't have a number it can't be what we want
    
    if (!dim(ME)[2] == length(interval.times)) {
      stop("Cannot find correct number of mass extinction probabilities")
    }
    
    posterior_probability <- apply(ME,2,function(x){sum(x != 0)})/dim(ME)[1]
    
  } else {
    ME <- posterior.summary[,grepl(parameter.name,names(posterior.summary))]
    ME <- ME[,grepl("[0-9]",names(ME))] #ME is a vector parameter, if it doesn't have a number it can't be what we want
    
    if (!dim(ME)[2] == length(interval.times)) {
      stop("Cannot find correct number of mass extinction probabilities")
    }
    
    posterior_probability <- 1 - as.numeric(ME[grepl("p0",row.names(posterior.summary)),])
    
  }

  # Get time running the way we want it
  if (interval.times[1] > 0) {
    interval.times <- -interval.times
  }

  # De-clutter x-axis
  oldest <- ceiling(max(-interval.times))
  pretty_times <- round(-seq(0,oldest,length.out=10))

  # Put BF thresholds on posterior probability scale
  two_ln_BF <- c(2,6,10)
  BF <- exp(two_ln_BF/2)
  prior_odds <- (prior.probability / (1 - prior.probability))
  BF_probs <- (BF*prior_odds)/(1 + BF*prior_odds)

#  plot(interval.times,posterior_probability,ylim=c(0,1),xaxt="n",xlab="Time before present",ylab="Posterior probability",main="",pch=16,col="grey60")
  # axis(side=1,at=interval.times,labels=-interval.times)
#  axis(side=1,at=pretty_times,labels=-pretty_times)
#  abline(h=BF_probs,lty=2)
#  axis(4,at=BF_probs,labels=two_ln_BF,las=1,tick=FALSE,line=-0.5)
#  abline(v=-66,lty=3)
#  text(-66, 1.0, "Cretaceous-Paleogene extinction", cex=0.65, pos=3,col="red")
#  abline(v=-201.3,lty=3)
#  text(-201.3, 1.0, "Triassic-Jurassic extinction", cex=0.65, pos=3,col="red")#

  nbins = length(posterior_probability)
  bp <- barplot(rev(posterior_probability),space=0,ylim=c(0,1.05),xaxt="n",xlab="",ylab="",main="",col="#4DAF4A",border="#4DAF4A")
  mtext(side=2, text="Posterior probability", line=2.25)
  labels <- pretty(c(0,oldest))
  labelsAt <- round((length(labels)-1):0*length(posterior_probability)/(length(labels)-1)) + round((oldest - max(labels))/length(posterior_probability))
  axis(side=1,at=labelsAt,labels=labels)

    ## add lines for BF
#    abline(h=BF_probs,lty=2)
  axis(4,at=BF_probs,labels=two_ln_BF,las=1,tick=FALSE,line=-0.5)

  polygon(c(-50,-50,350,350,-50), c(0, BF_probs[1], BF_probs[1], 0, 0), col = "grey70", border = NA)
  polygon(c(-50,-50,350,350,-50), c(BF_probs[1], BF_probs[2], BF_probs[2], BF_probs[1], BF_probs[1]), col = "grey80", border = NA)
  polygon(c(-50,-50,350,350,-50), c(BF_probs[2], BF_probs[3], BF_probs[3], BF_probs[2], BF_probs[2]), col = "grey90", border = NA)

  ## plot the bars again so that they are on top of the grey BF boxes
  barplot(rev(posterior_probability),space=0,ylim=c(0,1.05),xaxt="n",xlab="",ylab="",main="",col="#4DAF4A",border="#4DAF4A",add=TRUE)
  box()

  ## Add lines for major known mass extinctions
  abline(v=(oldest-66)/oldest*nbins,lty=3, lwd=2)
  text(bp[round((oldest-66)/oldest*nbins)], 0.95, "Cretaceous-Paleogene extinction", cex=0.5, pos=3,col="red")
  abline(v=(oldest-201.3)/oldest*nbins,lty=3, lwd=2)
  text(bp[round((oldest-201.3)/oldest*nbins)], 0.95, "Triassic-Jurassic extinction", cex=0.5, pos=3,col="red")
  
  
}


plotMEBFMulti <- function(posterior.summaries,interval.times,parameter.name,n.samples,prior.prob,plot.legend=TRUE,ylim=NA,geo.timeline=FALSE,geo.abbreviations=TRUE) {
  # recover()
  

  # Get time running the way we want it
  if (interval.times[1] > 0) {
    interval.times <- -interval.times
  }
  
  # De-clutter x-axis
  oldest <- ceiling(max(-interval.times))
  pretty_times <- round(-seq(0,oldest,length.out=10))
  
  # colors <- viridis_pal(alpha=90/256)(length(posterior.summaries[[1]]))

  ndatasets <- length(posterior.summaries)
  ntrajectories <- length(posterior.summaries[[1]])
  
  prior.odds <- prior.prob/(1 - prior.prob)
  
  all_BF <- vector("list",ndatasets)
  for (i in 1:ndatasets) {
    all_BF[[i]] <- list(ntrajectories)
    for (j in ntrajectories:1) {
      ME <- posterior.summaries[[i]][[j]][,grepl(parameter.name,names(posterior.summaries[[i]][[j]]))]
      ME <- ME[,grepl("[0-9]",names(ME))] #ME is a vector parameter, if it doesn't have a number it can't be what we want
      
      if (!dim(ME)[2] == length(interval.times)) {
        stop("Cannot find correct number of mass extinction probabilities")
      }
      
      posterior_probability <- 1 - as.numeric(ME[grepl("p0",row.names(ME)),])
      
      posterior_probability[posterior_probability == 0] <- 0.5/(n.samples+1)
      posterior_probability[posterior_probability == 1] <- (n.samples - 0.5)/(n.samples+1)
      
      all_BF[[i]][[j]] <- 2 * log((posterior_probability/(1 - posterior_probability))/prior.odds[j])
    }
  }
  
  nbins <- length(all_BF[[1]][[1]])
  
  two_ln_BF <- c(2,6,10)
  
  pch <- 0:(ndatasets-1)
  colors <- viridis_pal()(length(posterior.summaries[[1]]))
  
  par(mai=c(0.85,0.85,0.05,0.05),omi=rep(0.01,4),xpd=TRUE)
  yl <- range(unlist(all_BF))
  if ( !is.na(ylim[1]) ) {
    yl <- ylim
  }
  plot(NULL,NULL,xlim=range(interval.times),ylim=yl,xaxt="n",xlab="",ylab="2 log Bayes factor")
  labels <- pretty(c(0,oldest))

  l <- par("usr")[1]
  r <- par("usr")[2]
  b <- par("usr")[3]
  t <- par("usr")[4]
  
  # polygon(c(50,50,-350,-350,50), c(0, two_ln_BF[1], two_ln_BF[1], 0, 0), col = "grey70", border = NA)
  polygon(c(r,r,l,l,r), c(b, two_ln_BF[1], two_ln_BF[1], b, b), col = "grey70", border = NA)
  polygon(c(r,r,l,l,r), c(two_ln_BF[1], two_ln_BF[2], two_ln_BF[2], two_ln_BF[1], two_ln_BF[1]), col = "grey80", border = NA)
  polygon(c(r,r,l,l,r), c(two_ln_BF[2], two_ln_BF[3], two_ln_BF[3], two_ln_BF[2], two_ln_BF[2]), col = "grey90", border = NA)

  # polygon(c(50,50,-350,-350,50), c(0, -two_ln_BF[1], -two_ln_BF[1], 0, 0), col = "grey70", border = NA)
  # polygon(c(50,50,-350,-350,50), c(-two_ln_BF[1], -two_ln_BF[2], -two_ln_BF[2], -two_ln_BF[1], -two_ln_BF[1]), col = "grey80", border = NA)
  # polygon(c(50,50,-350,-350,50), c(-two_ln_BF[2], -two_ln_BF[3], -two_ln_BF[3], -two_ln_BF[2], -two_ln_BF[2]), col = "grey90", border = NA)
  
  for (i in 1:ndatasets) {
    for (j in ntrajectories:1) {
      points(interval.times,all_BF[[i]][[j]],pch=pch[i],col=colors[j])
    }
  }
  
  if ( plot.legend ) {
    legend("topright",legend=paste0("T",1:6),pch=pch,bty="n")
    legend("topleft",legend=(prior.prob*nbins),fill=colors,border=NA,bty="n")
  }
  

  # abline(v=-66,lty=3, lwd=2)
  # mtext(side=3,at=-71, "Cretaceous-Paleogene", cex=0.8,col="red",line=1.25)
  # abline(v=-201.3,lty=3, lwd=2)
  # mtext(side=3,at=-205, "Triassic-Jurassic", cex=0.8,col="red")
  # 
  # abline(v=-145,lty=3, lwd=2)
  # mtext(side=3,at=-140, "Jurassic-Cretaceous", cex=0.8,col="red")
  # abline(v=-33.9,lty=3, lwd=2)
  # mtext(side=3,at=-29, "Eocene-Oligocene", cex=0.8,col="red")
  
  if ( !geo.timeline ) {
    axis(side=1,at=-labels,labels=labels)
    mtext("Time before present (Ma)",1,line=2.25,cex=1)
  } else {
    # Geological timescale wrangling
    timescales <- NULL
    data(timescales,envir=environment())
    timescales <- timescales$ICS2015
    timescales <- timescales[timescales[,"Type"] == "Period",]
    timescales <- timescales[timescales[,"Start"] >= -max(interval.times),]
    timescales <- timescales[timescales[,"End"] <= -min(interval.times),]
    starts <- -timescales$End
    ends <- -timescales$Start
    if ( ends[length(ends)] < par("usr")[1] ) {
      ends[length(ends)] <- par("usr")[1]
    }
    if ( starts[1] > par("usr")[2] ) {
      starts[1] <- par("usr")[2]
    }
    
    rcols <- rgb(timescales$Col_R,timescales$Col_G,timescales$Col_B,maxColorValue=256)
    
    geo_box_top <-par("usr")[3]
    geo_box_bottom <- par("usr")[3]-0.075*(par("usr")[4]-par("usr")[3])
    rect(rev(ends),geo_box_top,rev(starts),geo_box_bottom,col=rev(rcols),border=NA)
    if ( geo.abbreviations ) {
      geo_names <- as.character(rev(timescales$Abbr))
      geo_names[geo_names %in% c("Q")] <- ""
      geo_names[geo_names == "P"] <- "Pg"
      geo_names[geo_names == "C"] <- "K"
    } else {
      geo_names <- as.character(rev(timescales$Name))
      # can't see these anyways
      geo_names[geo_names %in% c("Neogene","Quat.")] <- ""
      
    }
    
    text(x=(rev(ends)+rev(starts))/2,y=(geo_box_top+geo_box_bottom)/2,labels=geo_names)
    
    # And an axis label
    axis(side=1,at=-labels,labels=labels,pos=geo_box_bottom)
    mtext("Time before present (Ma)",1,line=3.25,cex=1)
    
  }
  
  box()
}

# Takes in matrix of estimated parameters, where each block is a dataset with different ME priors
plotParamCors <- function(param.matrix,ME.priors=c("0.0","0.1","0.5","1.0","2.0","5.0"),n.datasets=6,is.me=FALSE,main=NA) {
  # recover()

  n.me <- length(ME.priors)
  n.analyses = n.me * n.datasets


  cols <- viridis_pal(option="viridis")(100)

  cors <- cor(param.matrix)

  if ( is.me ) {
    cors[is.na(cors)] <- 1.0
  }

  # cors_cut <- apply(cors,1,cut,breaks=seq(-1.0,1.0,0.2),labels=FALSE,include.lowest=TRUE)

  layout(mat=cbind(1,2),widths=c(8,2))
  par(mai=rep(0.15,4),omi=rep(0.55,4))
  image(cors,x=1:n.analyses,y=1:n.analyses,col=cols,breaks=seq(-1.0,1.0,0.02),xaxt="n",yaxt="n",xlab="",ylab="")

  for (i in 1:n.datasets) {
    mtext(text=i,side=1,line=1,at=(i-1)*n.me+n.me/2)
    mtext(text=i,side=2,line=1,at=(i-1)*n.me+n.me/2)
  }

  if ( !is.na(main) ) {
    title(main)
  }

  plot.new()
  plot.window(xlim=c(0,2),ylim=c(0,100))

  for (i in 1:100) {
    polygon(x=c(0,0,1,1),y=c(i,i-1,i-1,i),border=cols[i],col=cols[i])
  }
  text(x=1.5,y=00.5,labels="-1.0")
  text(x=1.5,y=25.5,labels="-0.5")
  text(x=1.5,y=50.5,labels="0.0")
  text(x=1.5,y=75.5,labels="0.5")
  text(x=1.5,y=99.5,labels="1.0")

}
