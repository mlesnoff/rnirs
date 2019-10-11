sdod <- function(Xr, Xu = NULL, fm, ncomp = NULL, ...) {
  
  if(is.null(ncomp)) ncomp <- ncol(fm$Tr)
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- projscor(Xu, fm)

  sd <- dis(
    fm$Tr[, 1:ncomp, drop = FALSE], 
    Tu[, 1:ncomp, drop = FALSE], 
    diss = "mahalanobis", 
    ...
    )
  sdr <- sd$dr
  sdr$gh <- sdr$d^2 / sdr$ncomp
  
  od <- odis(Xr, Xu, fm, ncomp = ncomp, ...)
  odr <- od$dr
  
  odu <- sdu <- NULL
  if(!is.null(Xu)) {
    
    sdu <- sd$du
    sdu$gh <- sdu$d^2 / sdu$ncomp
    
    odu <- od$du
    
    }

  list(sdr = sdr, sdu = sdu, odr = odr, odu = odu)
  
  }


