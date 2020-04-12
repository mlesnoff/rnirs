pls.kernel <- function(X, Y, ncomp) {
  
  X <- .matrix(X)
  n <- nrow(X)
  zp <- ncol(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")   
  q <- ncol(Y)
  
  xmeans <- colMeans(X)
  X <- scale(X, center = xmeans, scale = FALSE)           
  
  ymeans <- colMeans(Y)
  Y <- scale(Y, center = ymeans, scale = FALSE) 

  nam <- paste("comp", 1:ncomp, sep = "")
  T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))            
  R <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam))  
  C <- matrix(nrow = q, ncol = ncomp, dimnames = list(colnames(Y), nam))            
  TT <- vector(length = ncomp)
  
  XY <- crossprod(X, Y)
  
  for(a in 1:ncomp) {
    
    if(q == 1) w <- XY
      else {
        w <- XY %*% svd(t(XY), nu = 1, nv = 0)$u
        }

    w <- w / sqrt(sum(w * w))
    
    r <- w
    if(a > 1) 
      for(j in 1:(a - 1)) 
        r <- r - sum(P[, j] * w) * R[, j]
    
    t <- X %*% r                              
    
    tt <- sum(t * t)
    
    p <- crossprod(X, t) / tt              
    
    beta <- crossprod(XY, r) / tt                 
    
    XY <- XY - tcrossprod(p, beta) * tt          
    
    T[, a] <- t
    P[, a] <- p
    W[, a] <- w
    R[, a] <- r
    C[, a] <- beta
    
    TT[a] <- tt
    
    }
  
  list(T = T, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = rep(1, n))
  
  }
