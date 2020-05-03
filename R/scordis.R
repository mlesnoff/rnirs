scordis <- function(fm) {
  
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  if(is.null(fm$Tr))
    fm$Tr <- fm$T
  
  ncomp <- dim(fm$Tr)[2]
  
  if(is.null(fm$TT))
    sigma <- fm$eigs
  else
    sigma <- fm$TT
  
  S <- diag(sigma, nrow = ncomp, ncol = ncomp)
  
  d <- dis(fm$Tr, fm$Tu, rep(0, ncomp), "mahalanobis", S)
  
  dr <- d$dr
  dr$gh <- dr$d^2 / dr$ncomp
  
  du <- NULL
  if(!is.null(fm$Tu)) {
    du <- d$du
    du$gh <- du$d^2 / du$ncomp
    }

  list(dr = dr, du = du)
  
  }


