blockreduct <- function(Xr, Yr = NULL, Xu = NULL, blocks, ncomp, ...) {
  
  nb <- length(blocks)
  if(length(ncomp) == 1) ncomp <- rep(ncomp, nb)
  
  zblocks <- data.frame(numcol = 1:sum(ncomp), bl = rep(1:nb, ncomp))
  
  res <- blocksel(Xr, blocks)
  Xr <- res$X
  if(!is.null(Xu))
    Xu <- blocksel(Xu, blocks)$X  
  
  for(i in 1:nb) {
    
    u <- res$blocks[[i]]
    
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