scordis <- function(fm, typcut = c("param", "mad", "boxplot")) {
  
  if(is.null(fm$Tr))
    fm$Tr <- fm$T
  
  typcut <- match.arg(typcut)
  
  ncomp <- dim(fm$Tr)[2]
  
  if(is.null(fm$TT))
    sigma <- fm$eig
  else
    sigma <- fm$TT
  
  S <- diag(sigma, nrow = ncomp, ncol = ncomp)
  
  res <- dis(fm$Tr, fm$Tu, rep(0, ncomp), "mahalanobis", S)
  
  dr <- res$dr
  
  d <- dr$d
  cutoff <- switch(
    typcut, 
    param = qchisq(p = .975, df = ncomp)^.5,
    mad = median(d) + 2.5 * mad(d),
    boxplot = {
      z <- fivenum(d)
      z <- z[4] + 1.5 * diff(z[c(2, 4)])
      max(d[d <= z])
      }
    )  

  dr$dstand <- d / cutoff
  dr$gh <- d^2 / ncomp
  
  du <- NULL
  if(!is.null(fm$Tu)) {
    du <- res$du
    du$dstand <- du$d / cutoff
    du$gh <- du$d^2 / ncomp
    }

  list(dr = dr, du = du, cutoff = cutoff)
  
  }


