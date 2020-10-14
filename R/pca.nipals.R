pca.nipals <- function(X, ncomp, weights = NULL, 
  tol = .Machine$double.eps^0.5, maxit = 100) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  ncomp <- min(ncomp, n, p)
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)
  
  sv <- vector(length = ncomp)
  T <- matrix(nrow = n, ncol = ncomp)
  P <- matrix(nrow = p, ncol = ncomp)
  
  niter <- vector(length = ncomp)
  
  for(a in 1:ncomp) {
    
    z <- .nipals(X, weights = weights)
  
    X <- X - tcrossprod(z$t, z$p)
    
    P[, a] <- z$p
    T[, a] <- z$t
    
    sv[a] <- z$sv
  
    niter[a] <- z$niter
    
    }
    
  eig <- sv^2         
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  colnames(P) <- colnames(T) <- paste("comp", 1:ncomp, sep = "") 
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, 
    xmeans = xmeans, weights = weights, niter = niter, T.ortho = TRUE)

  }

    
