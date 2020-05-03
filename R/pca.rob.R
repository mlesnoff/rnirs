pca.rob <- function(X, ncomp, nrep = 1, alpha = .75, h = 2) {
  
  P <- .simpp.hub(X, nrep = nrep)
  r <- .stahel(X, P)

  cutoff <- quantile(r, alpha)
    
  w1 <- ifelse(r < cutoff, 1, 0)
  fm <- pca.svd(X, ncomp = ncomp, weights = w1)
  
  w2 <- wdist(scordis(fm)$dr$d, h = h)
  
  w3 <- wdist(odis(fm, X)$dr$d, h = h)
  
  w <- w2 * w3
  #w <- w1 * w2 * w3
  fm <- pca.svd(X, ncomp = ncomp, weights = w)
  
  fm

  }
