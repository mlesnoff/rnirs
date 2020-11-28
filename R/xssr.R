xssr <- function(X, T, P, xmeans = rep(0, dim(P)[1])) {
  
  ### Unweighted SSR for Xfit
  ### (Including the component 'ncomp = 0')
  ### The output vector 'ssr' has length ncomp + 1

  X <- .matrix(X)
  T <- .matrix(T)
  P <- .matrix(P)
  ncomp <- ncol(T)
  
  X0 <- .center(X, xmeans)
  ssr <- sum(X0 * X0)
  for(a in 1:ncomp) {
    E <- X - xfit(T[, 1:a, drop = FALSE], P[, 1:a, drop = FALSE], xmeans)
    ssr[a + 1] <- sum(E * E)
    }

  ssr
  
  }
