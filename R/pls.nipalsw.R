pls.nipalsw <- function(X, Y, ncomp, weights = rep(1, nrow(X))) {
  
  X <- .matrix(X, prefix.colnam = "x")                                              
  n <- nrow(X)
  zp <- ncol(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")   
  zq <- ncol(Y)
  
  d <- weights / sum(weights)
  
  xmeans <- colSums(d * X)       # d * X = X * d = D %*% X

  ymeans <- colSums(d * Y) 
  
  X <- scale(X, center = xmeans, scale = FALSE)                                     
  
  Y <- scale(Y, center = ymeans, scale = FALSE) 
  
  nam <- paste("comp", 1:ncomp, sep = "")
  T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))           
  R <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam)) 
  C <- matrix(nrow = zq, ncol = ncomp, dimnames = list(colnames(Y), nam))           
  TT <- vector(length = ncomp)
  
  for(a in 1:ncomp) {
      
    XY <- crossprod(d * X, Y)     # XY = (D * X)' * Y = X' * D * Y
    
    if(zq == 1) {
      w <- XY
      w <- w / sqrt(sum(w * w))
      }
    else {
      z <- svd(XY, nu = 1, nv = 0)
      w <- z$u
      }
    
    t <- X %*% w
    
    tt <- sum(t * d * t)                
    
    p <- crossprod(d * X, t) / tt
    
    c <- crossprod(d * Y, t)  / tt
    
    X <- X - tcrossprod(t, p)
    
    Y <- Y - tcrossprod(t, c)
    
    T[, a] <- t

    W[, a] <- w

    P[, a] <- p
    C[, a] <- c
    
    TT[a] <- tt

    }
  
  R <- W %*% solve(crossprod(P, W))

  list(T = T, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = d)

  }
