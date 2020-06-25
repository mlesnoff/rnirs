pca.rob <- function(X, ncomp, nsim = 1500, alpha = .70, step2 = TRUE, ...) {
    
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(n < 2000 & (n < p))
    algo <- pca.eigenk
  else
    algo <- pca.eigen
  
  r <- out.stah(X, nsim = nsim)
  w1 <- .talworth(r, quantile(r, alpha))
  
  r <- out.eucl(X)
  w2 <- .talworth(r, quantile(r, alpha))
  
  w <- w1 * w2

  fm <- algo(X, ncomp = ncomp, weights = w)
  
  if(step2) {
    
    r <- out.pca(fm, X, ...)
    w <- .talworth(r, 1)
    
    fm <- algo(X, ncomp = ncomp, weights = w)
    
    }
  
  fm

  }
