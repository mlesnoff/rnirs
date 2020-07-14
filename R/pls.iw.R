pls.iw <- function(X, Y, ncomp, ncompw = 10, a = 3, 
                     tol = 1e-2, maxit = 10, weights = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)  
  
  scale.out <- TRUE
  r <- out.eucl(X, scale = scale.out)
  wx <- .tricube(r, a = a)
  
  wy <- rep(1, dim(X)[1])
  
  iter <- 0
  ztol <- 1
  ncompw <- min(ncompw, n, p)
  b <- rep(0, ncompw)
  while(ztol > tol & iter < maxit) {
    
    w <- wx * wy
    
    fm <- pls.kernel(X, Y, ncomp = ncompw, weights = weights * w)
    
    ztol <- .xnorm(b - c(fm$C))
    b <- c(fm$C)
    
    r <- out.eucl(fm$T, scale = scale.out)

    wx <- .tricube(r, a = a)
    
    r <- .resid.pls(fm, Y)$r
    wy <- .tricube(r / mad(r), a = a)
  
    iter <- iter + 1
    
    }
  
  fm <- pls.kernel(X, Y, ncomp = ncomp, weights = fm$weights)
  
  fm$niter <- iter
  
  fm

  }
