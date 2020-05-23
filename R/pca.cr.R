pca.cr <- function(X, ncomp, obj = mad, nsim = 0) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  simpp <- .simpp.hub
  
  xmeans <- .xmedspa(X)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  sv <- vector(length = ncomp)
  T <- matrix(nrow = n, ncol = ncomp)
  P <- matrix(nrow = p, ncol = ncomp)
  ndir <- NULL
  
  for(a in 1:ncomp) {
    
    zP <- simpp(X, nsim = nsim, seed = 1)
    ndir[a] <- dim(zP)[2]
    
    zT <- X %*% zP
    objval <- apply(zT, 2, obj)
    
    zp <- zP[, which.max(objval)]
    
    zt <- zT[, which.max(objval)]
  
    X <- X - zt %*% t(zp)
    
    P[, a] <- zp
    T[, a] <- zt
    
    sv[a] <- obj(zt)
    
    }  
  
  u <- rev(order(sv))
  P <- P[, u, drop = FALSE]
  T <- T[, u, drop = FALSE]
  sv <- sv[u]  
  
  eig <- sv^2         
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  colnames(P) <- colnames(T) <- paste("comp", 1:ncomp, sep = "") 
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, 
    xmeans = xmeans, weights = rep(1 / n, n), ndir = ndir)
  
  }




