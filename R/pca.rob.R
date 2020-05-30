pca.rob <- function(X, ncomp, nsim = 1000, alpha = .75, wod = FALSE) {
  
  X <- .matrix(X)
  n <- dim(X)[1]
  
  r <- out.stah(X, nsim = nsim)
  w1 <- .talworth(r, quantile(r, alpha))
  
  r <- out.eucl(X)
  w2 <- .talworth(r, quantile(r, alpha))
  
  w <- sqrt(w1 * w2)

  fm <- pca.svd(X, ncomp = ncomp, weights = w)

  if(wod) {
  
    d <- odis(fm, X)$dr$dstand
    w <- .talworth(d, 1) 
   
    fm <- pca.svd(X, ncomp = ncomp, weights = w)
    
    }

  fm

  }
