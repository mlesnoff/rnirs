pls.rannar <- function(X, Y, ncomp, weights = NULL) {
  
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
  
  #xmeans <- .xmean(X) 
  xmeans <- .xmean(X, weights = weights) 
  X <- .center(X, xmeans)

  ymeans <- .xmean(Y) 
  ymeans <- .xmean(Y, weights = weights) 
  Y <- .center(Y, ymeans)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  U <- T <- Tclass <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))           
  TT <- vector(length = ncomp)
  
  Xd <- sqrt(weights) * X
  Yd <- sqrt(weights) * Y
  
  XX <- tcrossprod(Xd)
  YY <- tcrossprod(Yd)
  
  XY <- XX %*% YY  
  
  I <- diag(n)

  for(a in 1:ncomp) {
    
    t <- .eigpow(XY)$v

    u <- YY %*% t
  
    utemp <-  u / sum(u * t)
    wtw <- c(crossprod(utemp, XX) %*% utemp)
    tclass <- t * sqrt(wtw) / sqrt(weights)
    
    tt <- sum(weights * tclass * tclass)  
    
    G <- I - tcrossprod(t)
    XX <- G %*% (XX) %*% G 
    YY <- G %*% YY %*% G
    XY <- XX %*% YY
    
    T[, a] <- t
    Tclass[, a] <- tclass
    U[, a] <- u

    TT[a] <- tt
    
    }

  W <- crossprod(Xd, U)
  W <- .scale(W, scale = .xnorm(W))
  
  Z <- solve(crossprod(T))
  Z <- .scale(Z, scale = sqrt(TT))
  
  P <- crossprod(Xd, T) %*% Z
  C <- crossprod(Yd, T) %*% Z

  R <- W %*% solve(crossprod(P, W))
  
  list(T = Tclass, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = weights, U = U)
  
  }
