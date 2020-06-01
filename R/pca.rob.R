pca.rob <- function(X, ncomp, nsim = 1500, alpha = .75, step2 = TRUE) {
  
  X <- .matrix(X)
  n <- dim(X)[1]
  
  r <- out.stah(X, nsim = nsim)
  w1 <- .talworth(r, quantile(r, alpha))
  
  r <- out.eucl(X)
  w2 <- .talworth(r, quantile(r, alpha))
  
  w <- w1 * w2

  fm <- pca.svd(X, ncomp = ncomp, weights = w)
  
  if(step2) {
    
    zsd <- scordis(fm)$dr$dstand
    zod <- odis(fm, X)$dr$dstand
    
    r <- sqrt(.5 * zsd^2 + .5 * zod^2)
    w <- .talworth(r, quantile(r, alpha))
    
    fm <- pca.svd(X, ncomp = ncomp, weights = w)
    
    }
  
  fm

  }
