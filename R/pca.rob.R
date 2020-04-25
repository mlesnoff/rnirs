pca.rob <- function(X, ncomp, w1 = c("pp", "sph"), 
  nrep = 1, ncomp.sph = 10, cri = 2.5, alpha = NULL, h = 2) {
  
  w1 <- match.arg(w1)
  
  if(w1 == "pp") {
    
    P <- .simpp.hub(X, nrep = nrep)
    r <- .stahel(X, P)

    }
  
  if(w1 == "sph") {
    
    ncomp.sph <- min(ncomp.sph, dim(X)[2])
    
    fm <- pca.sph(X, ncomp = ncomp.sph)
    r <- scordis(fm)$dr$d
    r <- abs(r - median(r)) / mad(r)

    }
   
  if(is.null(alpha))
    cutoff <- cri
  else
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
