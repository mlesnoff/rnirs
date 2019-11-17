blocksopca <- function(Xr, Xu = NULL, blocks, ncomp, ...) {
  
  nbl <- length(blocks)
  
  if(length(ncomp) == 1) ncomp <- rep(ncomp, nbl)
  
  zblocks <- data.frame(numcol = 1:sum(ncomp), bl = rep(1:nbl, ncomp))
  
  newdat <- blocksel(Xr, blocks)
  Xr <- newdat$X
  newblocks <- newdat$blocks
  
  ### TRICK WHEN Xu IS NULL
  nullXu <- FALSE
  if(is.null(Xu)) {
    Xu <- Xr[1, , drop = FALSE]
    nullXu <- TRUE
    }
  else
    Xu <- blocksel(Xu, blocks)$X
  ### END
  
  m <- nrow(Xu)
  
  fm <- pca(
    Xr[, newblocks[[1]], drop = FALSE],
    Xu[, newblocks[[1]], drop = FALSE],
    ncomp = ncomp[1], ...
    )
  Tr <- fm$Tr
  Tu <- fm$Tu
  
  blocks[[1]] <- zblocks$numcol[zblocks$bl == 1]

  for(i in 2:nbl) {
  
    z <- orthog(Tr, Xr[, newblocks[[i]], drop = FALSE], fm$weights)
    
    fm <- pca(
      z$Y,
      Xu[, newblocks[[i]], drop = FALSE] - cbind(rep(1, m), Tu) %*% z$b,
      ncomp = ncomp[i], ...
      )
  
    Tr <- cbind(Tr, fm$Tr)
    Tu <- cbind(Tu, fm$Tu)
    
    blocks[[i]] <- zblocks$numcol[zblocks$bl == i]

    }
  
  if(nullXu) Tu <- NULL
  
  list(Tr = Tr, Tu = Tu, blocks = blocks)  

  }