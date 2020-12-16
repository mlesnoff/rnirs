pls_nipals <- function(X, Y, ncomp, weights = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  zp <- zdim[2]
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")   
  q <- dim(Y)[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights) 
  X <- .center(X, xmeans)

  ymeans <- .xmean(Y, weights = weights) 
  Y <- .center(Y, ymeans)
  
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
    
    tt <- sum(weights * t * t)                
    
    c <- crossprod(weights * Y, t)  / tt
    
    p <- crossprod(weights * X, t) / tt
    
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
    xmeans = xmeans, ymeans = ymeans, weights = weights, T.ortho = TRUE)

  }
