scordis <- function(fm, out = c("mad", "sd", "boxplot"), cri = 3) {
  
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  if(is.null(fm$Tr))
    fm$Tr <- fm$T
  
  ncomp <- dim(fm$Tr)[2]
  
  if(is.null(fm$TT))
    sigma <- fm$xss
  else
    sigma <- fm$TT
  
  S <- diag(sigma, nrow = ncomp, ncol = ncomp)
  
  d <- dis(mu = rep(0, ncomp), Xr = fm$Tr, Xu = fm$Tu, diss = "mahalanobis", 
    sigma = S, out = out, cri = cri)
  
  dr <- d$dr
  dr$gh <- dr$d^2 / dr$ncomp
  
  du <- NULL
  if(!is.null(fm$Tu)) {
    du <- d$du
    du$gh <- du$d^2 / du$ncomp
    }

  list(dr = dr, du = du, cut = d$cut)
  
  }


