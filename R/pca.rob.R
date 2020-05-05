pca.rob <- function(X, ncomp, alpha = .75, nrep = 0) {
  
  xmeds <- apply(X, 2, median)
  xmads <- apply(X, 2, mad)
  zX <- scale(X, center = xmeds, scale = xmads)
  d <- .stahel(zX, .simpp.hub(zX, nrep = nrep))
  cutoff <- quantile(d, alpha)
  w1 <- ifelse(d < cutoff, 1, 0)

  fm <- pca.svd(X, ncomp = ncomp, weights = w1)
  
  z <- odis(fm, X)
  d <-  z$dr$d
  #cutoff <- quantile(d, alpha)
  cutoff <- z$cutoff
  w2 <- ifelse(d < cutoff, 1, 0)
  
  w <- w2
  #w <- w1 * w2
  
  fm <- pca.svd(X, ncomp = ncomp, weights = w)
  
  fm

  }
