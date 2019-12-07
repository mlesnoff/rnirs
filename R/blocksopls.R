blocksopls <- function(Xr, Yr, Xu = NULL, blocks, ncomp, ...) {
  
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
  
  n <- nrow(Xr)
  m <- nrow(Xu)
  
  fm <- pls(
    Xr[, newblocks[[1]], drop = FALSE],
    Yr,
    Xu[, newblocks[[1]], drop = FALSE],
    ncomp = ncomp[1], ...
    )
  Tr <- fm$Tr
  Tu <- fm$Tu
  Ymeansr <- matrix(rep(fm$ymeans, n), nrow = n, byrow = TRUE)
  Ymeansu <- matrix(rep(fm$ymeans, m), nrow = m, byrow = TRUE)
  beta <- t(fm$C)
  Fitr <- Ymeansr + fm$Tr %*% beta
  Fitu <- Ymeansu + fm$Tu %*% beta

  blocks[[1]] <- zblocks$numcol[zblocks$bl == 1]

  for(i in 2:nbl) {
  
    z <- orthog(Tr, Xr[, newblocks[[i]], drop = FALSE], fm$weights)
    
    fm <- pls(
      z$Y,
      Yr - Fitr,
      Xu[, newblocks[[i]], drop = FALSE] - cbind(rep(1, m), Tu) %*% z$b,
      ncomp = ncomp[i], ...
      )
  
    Tr <- cbind(Tr, fm$Tr)
    Tu <- cbind(Tu, fm$Tu)
    
    Ymeansr <- matrix(rep(fm$ymeans, n), nrow = n, byrow = TRUE)
    Ymeansu <- matrix(rep(fm$ymeans, m), nrow = m, byrow = TRUE)
    beta <- t(fm$C)
    Fitr <- Fitr + Ymeansr + fm$Tr %*% beta
    Fitu <- Fitu + Ymeansu + fm$Tu %*% beta
    
    blocks[[i]] <- zblocks$numcol[zblocks$bl == i]

    }
  
  if(nullXu) Fitu <- Tu <- NULL
  
  list(Tr = Tr, Tu = Tu, Fitr = Fitr, Fitu = Fitu, 
    blocks = blocks, ncomp = ncomp)  

  }