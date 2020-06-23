pca.rob <- function(X, ncomp, nsim = 1500, alpha = .70, step2 = TRUE, kern = NULL, ...) {
    
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  r <- out.stah(X, nsim = nsim)
  w1 <- .talworth(r, quantile(r, alpha))
  
  r <- out.eucl(X)
  w2 <- .talworth(r, quantile(r, alpha))
  
  w <- w1 * w2

  fm <- pca.eigen(X, ncomp = ncomp, weights = w, kern = kern)
  
  if(step2) {
    
    r <- out.pca(fm, X, ...)
    w <- .talworth(r, 1)
    
    fm <- pca.eigen(X, ncomp = ncomp, weights = w, kern = kern)
    
    }
  
  fm

  }
