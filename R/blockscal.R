blockscal <- function(Xr, Xu = NULL, blocks, weights = rep(1, nrow(Xr))) {
  
  nbl <- length(blocks)
  
  d <- weights / sum(weights)

  res <- blocksel(Xr, blocks)
  Xr <- res$X
  blocks <- res$blocks
  
  xdisptot <- rep(NA, nbl)
  for(i in 1:nbl) {
    
    #xdisptot[i] <- sum(
    #  apply(Xr[, blocks[[i]], drop = FALSE], MARGIN = 2, 
    #    FUN = function(x) sqrt(.varw(x)))
    #  )
    
    xdisptot[i] <- sqrt(
      sum(
      apply(Xr[, blocks[[i]], drop = FALSE], MARGIN = 2, FUN = .varw, 
        weights = weights)
        )
      )
    
    Xr[, blocks[[i]]] <- Xr[, blocks[[i]], drop = FALSE] / xdisptot[i]
      
    }

  if(!is.null(Xu)) {
    
    Xu <- blocksel(Xu, blocks)$X
    for(i in 1:nbl)  
      Xu[, blocks[[i]]] <- Xu[, blocks[[i]], drop = FALSE] / xdisptot[i]
    
    }
    
  list(Xr = Xr, Xu = Xu, blocks = blocks, xdisptot = xdisptot)  

  }