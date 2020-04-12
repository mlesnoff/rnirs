pca.nipalsw <- function(X, ncomp, weights = rep(1, nrow(X)), 
  tol = .Machine$double.eps^0.5, maxit = 100) {
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  xmeans <- .xmeans(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  sv <- vector(length = ncomp)
  T <- matrix(nrow = n, ncol = ncomp)
  P <- matrix(nrow = p, ncol = ncomp)
  
  niter <- vector(length = ncomp)
  
  for(a in 1:ncomp) {
    
    z <- .nipals(X, weights = weights)
  
    X <- X - z$t %*% t(z$p)
    
    P[, a] <- z$p
    T[, a] <- z$t
    
    sv[a] <- z$sv
    ## = norm of the score t (in metric D)
    ## = .xnorms(t, weights = weights)
    ## = sqrt(sum(weight * t * t))
  
    niter[a] <- z$niter
    
    }
    
  xss <- sv^2         
  ## = eigenvalues of X'X
  ## = colSums(weights T * T)
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  colnames(P) <- colnames(T) <- paste("comp", 1:ncomp, sep = "") 
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, 
    xmeans = xmeans, weights = rep(1, n), niter = niter)

  }

    
