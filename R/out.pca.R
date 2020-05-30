out.pca <- function(X, ncomp, algo = pca.rob, ...) {
  
  fm <- algo(X, ncomp = ncomp, ...)
  
  zsd <- scordis(fm)$dr$dstand
  zod <- odis(fm, X)$dr$dstand
  
  r <- sqrt(.5 * zsd^2 + .5 * zod^2)
  
  r

  }