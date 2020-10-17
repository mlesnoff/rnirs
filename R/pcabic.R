pcabic <- function(X, ncomp, algo = NULL, segm, 
                   naive = FALSE, print = FALSE, ...) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  zn <- n * p  
  
  ncomp <- min(ncomp, n, p)
  zncomp <- 0:ncomp
  
  if(naive) {
    z <- pca(X, ncomp = ncomp, algo = algo, ...)$explvar
    dof <- n * zncomp + p * zncomp + p - zncomp - zncomp^2    
    } else {
      z <- pcadof(X, ncomp, algo, segm, print, ...)
      dof <- z$dof.mod
      }
  ssr <- z$ssr
  dof.ssr <- zn - dof

  u <- data.frame(ncomp = zncomp, ssr = ssr,
                  dof.mod = dof, dof.ssr = dof.ssr)
  u$msep <- ssr / dof.ssr
  u$bic <- zn * log(ssr) + log(zn) * dof
  u$aic <- zn * log(ssr) + 2 * dof
  s2.0 <- ssr[ncomp] / (zn - dof[ncomp])
  u$cp <- ssr / s2.0 + 2 * dof
  u$fpe <- (zn + dof) / (zn - dof) * ssr  
  u$gcv <- zn * ssr / (dof.ssr)^2
  
  rms <- sqrt(ssr / zn)
  ## Imbedded Error IE = sqrt(R^2 - RMS^2)    (Malinowski 1977 a & b)
  ## For IE calculation, the strict denominator in RSD is "n * (p - a)". 
  ## With such denominator, IE = sqrt(p / a) * RSD (= classical expression).
  ## But this denominator is not a correct DoF for the residual error SSR.
  ## Below we consider the "real" DoF (not naïve or naïve) for SSR
  rsd <- sqrt(ssr / dof.ssr)
  u$ie <- sqrt(rsd^2 - rms^2) 

  u
  
  }
