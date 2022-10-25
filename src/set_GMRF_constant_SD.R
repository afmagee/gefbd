rGMRF <- function(n,dim,sd) {
  t(sapply(1:n,function(i){
    c(0,cumsum(rnorm(dim-1,0,sd)))
  }))
}

estGMRFMinMaxRatio <- function(nsim,grid.size,global.scale,summary.function) {
  draws <- rGMRF(nsim,grid.size,global.scale)
  magnitudes <- apply(draws,1,function(x){exp(max(x) - min(x))})
  return(summary.function(magnitudes))
}

setGlobalScale <- function(alpha,fold.change,grid.size,nsim=1e5) {
  q_alpha <- function(x){quantile(x,1 - alpha)}
  f <- function(x) {
    q <- estGMRFMinMaxRatio(nsim=nsim,grid.size=grid.size,global.scale=x,summary.function=q_alpha)
    (q - fold.change)^2
  }
  optimize(f,c(0,1))
}

set.seed(42)
gs <- setGlobalScale(alpha=0.5,fold.change=exp(1),50,nsim=1e5)
gs

m <- sapply(1:1e5,function(i){
  x <- rGMRF(1,50,0.105)
  exp(max(x) - min(x))
})

hist(m,breaks=200,xlim=c(0,10))
summary(m)
quantile(m,c(0.9,0.95,0.99))

set.seed(47)
setGlobalScale(alpha=0.5,fold.change=exp(1),100,nsim=1e5)

set.seed(8472)
setGlobalScale(alpha=0.5,fold.change=exp(1),25,nsim=1e5)
