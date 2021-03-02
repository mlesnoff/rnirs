outsdod <- function(
    fm, X,
    ncomp = NULL, 
    robust = FALSE, alpha = .01
    ) {
  
    zsd <- scordis(fm, ncomp = ncomp, robust = robust, alpha = alpha)$dr$dstand
    
    zod <- odis(fm, X, ncomp = ncomp, robust = robust, alpha = alpha)$dr$dstand
    
    r <- sqrt(.5 * zsd^2 + .5 * zod^2)
  
    r

    }