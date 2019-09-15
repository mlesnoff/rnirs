sdod <- function(fm, ncomp = NULL, ...) {
  
  if(is.null(ncomp)) ncomp <- ncol(fm$Tr)
  
  Tr <- fm$Tr[, 1:ncomp, drop = FALSE]
  Tu <- fm$Tu[, 1:ncomp, drop = FALSE]
  
  sd <- dis(Tr, Xu = Tu, diss = "mahalanobis", ...)
  od <- odis(fm$Xr, fm, fm$Xu, ncomp = ncomp, ...)
  
  sdr <- sd$dr
  sdr$gh <- sdr$d^2 / sdr$ncomp
  
  sdu <- sd$du
  sdu$gh <- sdu$d^2 / sdu$ncomp
  
  odr <- od$dr
  odu <- od$du

  list(sdr = sdr, sdu = sdu, odr = odr, odu = odu)
  
  }


