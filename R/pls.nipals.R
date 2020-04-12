pls.nipals <- function(X, Y, ncomp) {
  
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

  for(a in 1:ncomp) {
    
    ## Weights w and c that maximize cov(t, u) [same as cov(t, u)^2]
    ## with t = X w, u = Y c, ||w|| = ||c|| = 1
    ## and X.approx = t p'
  
    XY <- crossprod(X, Y)
    ## = t(X) %*% Y
    
    if(q == 1) {
      w <- XY
      w <- w / sqrt(sum(w * w))
      }
    else {
      ## 1st eigenvector of X'YY'X
      w <- svd(XY, nu = 1, nv = 0)$u
      ## = .nipals(XY %*% t(XY))$p
      }
    
    t <- X %*% w
    
    tt <- sum(t * t) 
    
    ## Regression of Y on t
    beta <- crossprod(Y, t)  / tt
    ## = vector 1xq of the coefficients
    ## c = beta / ||beta|| = 1st eigenvector of Y'XX'Y
    ## Y-score u = crossprod(Y, c)
    
    ## Regression of X on t
    p <- crossprod(X, t) / tt
    ## = vector px1 of the coefficients
    
    ## X-deflation
    ## The Y-dfeltaion is optional
    X <- X - tcrossprod(t, p)
    
    T[, a] <- t
    W[, a] <- w
    P[, a] <- p
    ## For consistency with usual notations
    C[, a] <- beta
    
    TT[a] <- tt
    
    }
  
  R <- W %*% solve(crossprod(P, W))

  list(T = T, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = rep(1, n))

  }
