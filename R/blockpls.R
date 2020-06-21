blockpls <- function(Xr, Yr = NULL, Xu = NULL, blocks, colblocks = NULL, ncomp, ...) {
  
  if(!is.null(colblocks)) {
    lev <- levels(as.factor(colblocks))
    nlev <- length(lev)
    blocks <- vector(mode = "list", length = nlev)
    for(i in 1:nlev)
      blocks[[i]] <- which(colblocks == lev[i])  
    }
  
  nbl <- length(blocks)
  
  if(length(ncomp) == 1) ncomp <- rep(ncomp, nbl)
  
  zblocks <- data.frame(numcol = 1:sum(ncomp), bl = rep(1:nbl, ncomp))
  
  newdat <- blocksel(Xr, blocks)
  Xr <- newdat$X
  newblocks <- newdat$blocks
  
  if(!is.null(Xu))
    Xu <- blocksel(Xu, blocks)$X  
  
  for(i in 1:nbl) {
    
    u <- newdat$blocks[[i]]
    
    if(is.null(Yr))
      fm <- pca(Xr[, u, drop = FALSE], Xu[, u, drop = FALSE], ncomp[i], ...)
    else
      fm <- pls(Xr[, u, drop = FALSE], Yr, Xu[, u, drop = FALSE], ncomp[i], ...)
    
    if(i == 1) {
      Tr <- fm$Tr
      Tu <- fm$Tu
      } else {
        Tr <- cbind(Tr, fm$Tr)
        Tu <- cbind(Tu, fm$Tu)
        }
    
    blocks[[i]] <- zblocks$numcol[zblocks$bl == i]
    
    }
  
  list(Tr = Tr, Tu = Tu, blocks = blocks)  

  }