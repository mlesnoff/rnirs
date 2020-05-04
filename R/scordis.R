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
  cutoff <- qchisq(p = .975, df = ncomp)^.5
  dr$dstand <- dr$d / cutoff
  dr$gh <- dr$d^2 / dr$ncomp
  
  du <- NULL
  if(!is.null(fm$Tu)) {
    du <- d$du
    du$dstand <- du$d / cutoff
    du$gh <- du$d^2 / du$ncomp
    }

  list(dr = dr, du = du, cutoff = cutoff)
  
  }


