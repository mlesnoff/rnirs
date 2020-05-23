pca.rob <- function(X, ncomp, nsim = 1000, alpha = .75, w2 = FALSE) {
  
  d <- stahel(X, nsim = nsim)
  cutoff <- quantile(d, alpha)
  w1 <- ifelse(d > cutoff, 0, 1)
  
  fm <- pca.svd(X, ncomp = ncomp, weights = w1)
  
  if(w2) {
  
    z <- odis(fm, X)
    odstand <-  z$dr$dstand
    w2 <- ifelse(odstand > 1, 0, 1)
    
    fm <- pca.svd(X, ncomp = ncomp, weights = w2)
    
    }
  
  fm

  }
