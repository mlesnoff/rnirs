stahel <- function(X, scale = TRUE, nsim = 3000) {
  
  simpp <- .simpp.hub
  
  if(scale) {
    zmu <- apply(X, 2, median)
    zs <- apply(X, 2, mad)
    X <- scale(X, center = zmu, scale = zs)
    }
    
  P <- simpp(X, nsim = nsim, seed = 1)
  
  T <- X %*% P

  mu <- apply(T, 2, median)
  s <- apply(T, 2, mad)
  T <- scale(T, center = mu, scale = s)
  
  r <- apply(abs(T), 1, max)
  
  r  

  }