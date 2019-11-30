scordis <- function(fm, out = c("mad", "sd", "boxplot"), 
  cri = 3) {
  
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  ncomp <- ncol(fm$Tr)
  
  d <- dis(NULL, fm$Tr, fm$Tu, diss = "mahalanobis", out = out, cri = cri)
  
  dr <- d$dr
  dr$gh <- dr$d^2 / dr$ncomp
  
  du <- NULL
  if(!is.null(fm$Tu)) {
    du <- d$du
    du$gh <- du$d^2 / du$ncomp
    }

  list(dr = dr, du = du, cut = d$cut)
  
  }


