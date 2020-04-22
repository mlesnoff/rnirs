blockscal <- function(Xr, Xu = NULL, blocks, weights = NULL) {
  
  nbl <- length(blocks)
  
  n <- dim(Xr)[1]

  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  newdat <- blocksel(Xr, blocks)
  Xr <- newdat$X
  newblocks <- newdat$blocks
  
  if(!is.null(Xu))
    Xu <- blocksel(Xu, blocks)$X  
  
  xdisptot <- rep(NA, nbl)
  for(i in 1:nbl) {
    
    z <- .xvar(Xr[, newblocks[[i]], drop = FALSE], weights = weights)
    
    xdisptot[i] <- sqrt(sum(z))
    
    Xr[, newblocks[[i]]] <- Xr[, newblocks[[i]], drop = FALSE] / xdisptot[i]
      
    }

  if(!is.null(Xu)) {
    
    for(i in 1:nbl)  
      Xu[, newblocks[[i]]] <- Xu[, newblocks[[i]], drop = FALSE] / xdisptot[i]
    
    }
    
  list(Xr = Xr, Xu = Xu, blocks = newblocks, xdisptot = xdisptot)  

  }