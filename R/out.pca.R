out.pca <- function(fm, X, ...) {
  
  zsd <- scordis(fm, ...)$dr$dstand
  zod <- odis(fm, X, ...)$dr$dstand
  
  r <- sqrt(.5 * zsd^2 + .5 * zod^2)
  
  r

  }