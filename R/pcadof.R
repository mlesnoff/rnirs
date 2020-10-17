pcadof <- function(X, ncomp, algo = NULL, segm,
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
    n = rep(zn, ncomp + 1),
    dof.mod = n * zncomp + round(dfa)
    )
  z$dof.mod.naive <- n * zncomp + p * zncomp + p - zncomp - zncomp^2
  z$dof.ssr <- zn - z$dof.mod
  
  z
  
  }
