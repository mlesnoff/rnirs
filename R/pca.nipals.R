pca.nipals <- function(X, ncomp, weights = NULL, 
  tol = .Machine$double.eps^0.5, maxit = 100) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
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
    ## = norms of the scores T in metric D
    ## = .xnorm(T, weights = weights)
    ## = sqrt(colSums(weights * T * T))
  
    niter[a] <- z$niter
    
    }
    
  xss <- sv^2         
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  ## = variances of scores T in metric D
  ## = colSums(weights * T * T)  
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  colnames(P) <- colnames(T) <- paste("comp", 1:ncomp, sep = "") 
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, 
    xmeans = xmeans, weights = weights, niter = niter)

  }

    
