pcadof <- function(X, ncomp, segm, algo = NULL, 
                      print = FALSE, ...) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  ncomp <- min(ncomp, n, p)
  zncomp <- 0:ncomp
  
  ssr <- pca(X, ncomp = ncomp, algo = algo, ...)$explvar$ssr
  ssrcv <- pcacv(X, ncomp, segm, algo, print, ...)$mse$ssr
  
  zn <- n * p
  dfa <- n * (p - zncomp) * (1 - ssr / ssrcv)
  z <- data.frame(
    ncomp = zncomp,
    ssr = ssr,
    ssrcv = ssrcv,
    dof.mod = n * zncomp + round(dfa)
    )
  z$dof.mod.naive <- n * zncomp + p * zncomp + p - zncomp - zncomp^2
  z$dof.ssr <- zn - z$dof.mod

  u <- data.frame(ncomp = zncomp)
  u$msep <- ssr / z$dof.ssr
  dof <- z$dof.mod
  s2.0 <- ssr[ncomp] / (zn - dof[ncomp])
  u$cp <- ssr / s2.0 + 2 * dof
  u$fpe <- (zn + dof) / (zn - dof) * ssr  
  u$aic <- zn * log(ssr) + 2 * dof
  u$bic <- zn * log(ssr) + log(zn) * dof
  u$gcv <- zn * ssr / (z$dof.ssr)^2

  list(dof = z, mse = u)
  
  }
