pls.kernelw <- function(X, Y, ncomp, weights) {
  
  X <- .matrix(X, prefix.colnam = "x")                                              
  n <- nrow(X)
  zp <- ncol(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")   
  zq <- ncol(Y)
  
  d <- weights / sum(weights)
  
  Xd <- d * X   # d * X = D %*% X
  
  xmeans <- colSums(Xd) 

  ymeans <- colSums(Y * d)   
  
  X <- scale(X, center = xmeans, scale = FALSE)                                     
  
  Y <- scale(Y, center = ymeans, scale = FALSE) 
  
  nam <- paste("comp", 1:ncomp, sep = "")
  T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))           
  R <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam)) 
  C <- matrix(nrow = zq, ncol = ncomp, dimnames = list(colnames(Y), nam))           
  TT <- vector(length = ncomp)
  
  XY <- crossprod(Xd, Y)     # XY = (D * X)' * Y = X' * D * Y
  
  for(a in 1:ncomp) {
    
    if(zq == 1) w <- XY
      else {
        z <- svd(t(XY), nu = 1, nv = 0)
        w <- XY %*% z$u
        }

    w <- w / sqrt(sum(w * w))
    
    r <- w
    if(a > 1)
      for(j in 1:(a - 1)) r <- r - sum(P[, j] * w) * R[, j]
    
    t <- X %*% r 
    
    tt <- sum(t * d * t)     
    
    p <- crossprod(Xd, t) / tt 
    
    c <- crossprod(XY, r) / tt
    
    XY <- XY - tcrossprod(p, c) * tt  
    
    T[, a] <- t
    P[, a] <- p
    W[, a] <- w
    R[, a] <- r
    C[, a] <- c
    
    TT[a] <- tt
    
    }

  list(T = T, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = d)
  
  }
