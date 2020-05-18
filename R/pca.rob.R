pca.rob <- function(X, ncomp, nsim = 3000, alpha = .75) {
  
  d <- stahel(X, scale = TRUE, nsim = nsim)
  cutoff <- quantile(d, alpha)
  w1 <- ifelse(d < cutoff, 1, 0)
  
  fm <- pca.svd(X, ncomp = ncomp, weights = w1)
  
  z <- odis(fm, X)
  d <-  z$dr$d
  cutoff <- z$cutoff
  w2 <- ifelse(d < cutoff, 1, 0)
  
  fm <- pca.svd(X, ncomp = ncomp, weights = w2)
  
  fm

  }
