pls.nipals <- function(X, Y, ncomp, weights = rep(1, nrow(X))) {
  
  X <- .matrix(X)
  n <- nrow(X)
  zp <- ncol(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")   
  q <- ncol(Y)
  
  xmeans <- .xmeans(X, weights = weights) 
  X <- scale(X, center = xmeans, scale = FALSE)                                     

  ymeans <- .xmeans(Y, weights = weights) 
  Y <- scale(Y, center = ymeans, scale = FALSE)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))           
  R <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam)) 
  C <- matrix(nrow = q, ncol = ncomp, dimnames = list(colnames(Y), nam))           
  TT <- vector(length = ncomp)
  
  for(a in 1:ncomp) {
      
    XY <- crossprod(weights * X, Y)
    # = t(D %*% X) %*% Y = t(X) %*% D %*% Y
    
    if(q == 1) {
      w <- XY
      w <- w / sqrt(sum(w * w))
      }
    else {
      w <- svd(XY, nu = 1, nv = 0)$u
      }
    
    t <- X %*% w
    
    tt <- sum(weights * t *t)                
    
    beta <- crossprod(weights * Y, t)  / tt
    
    p <- crossprod(weights * X, t) / tt
    
    X <- X - tcrossprod(t, p)
    
    T[, a] <- t
    W[, a] <- w
    P[, a] <- p
    C[, a] <- beta
    
    TT[a] <- tt

    }
  
  R <- W %*% solve(crossprod(P, W))

  list(T = T, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = weights)

  }
