blockscal <- function(Xr, Xu = NULL, blocks, stat = sd) {
  
  nb <- length(blocks)
  
  res <- blocksel(Xr, blocks)
  Xr <- res$X
  
  xdisptot <- rep(NA, nb)
  for(i in 1:nb) {
    
    xdisptot[i] <- sum(apply(Xr[, res$blocks[[i]], drop = FALSE], MARGIN = 2, FUN = stat))
    Xr[, res$blocks[[i]]] <- Xr[, res$blocks[[i]], drop = FALSE] / xdisptot[i]
      
    }

  if(!is.null(Xu)) {
    
    Xu <- blocksel(Xu, blocks)$X
    for(i in 1:nb)  
      Xu[, res$blocks[[i]]] <- Xu[, res$blocks[[i]], drop = FALSE] / xdisptot[i]
    
    }
    
  list(Xr = Xr, Xu = Xu, xdisptot = xdisptot, blocks = res$blocks)  

  }