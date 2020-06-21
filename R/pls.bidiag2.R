pls.bidiag2 <- function(X, Y, ncomp, weights = NULL) {
  
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
  TT <- vector(length = ncomp)
  beta <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam)) 
  
  B <- matrix(0, nrow = ncomp, ncol = 2)
  row.names(B) <- 1:ncomp
  
  XY <- crossprod(weights * X, Y)
  w <- XY
  w <- w / sqrt(sum(w * w))
  
  t <- X %*% w
  
  #tt <- sum(weights * t * t) 
  #p <- crossprod(weights * X, t) / tt
  
  rho <- sqrt(sum(t * t))
  t <- t / rho

  d <- w / rho
  
  T[, 1] <- t
  W[, 1] <- w
  #P[, 1] <- p
  #TT[1] <- tt
  
  B[1, 1] <- rho
  
  beta[, 1] <- sum(t * y) * d
  
  for(a in 2:ncomp) {
    
    w <- crossprod(X, t) - rho * w
    
    zW <-  W[, 1:(a - 1), drop = FALSE]
    w <- w - zW %*% crossprod(zW, w)

    theta <- sqrt(sum(w * w))
    w <- w / theta

    t <- X %*% w - theta * t
    
    zT <- T[, 1:(a - 1), drop = FALSE]
    t <- t - zT %*% crossprod(zT, t)
  
    #tt <- sum(weights * t * t) 
    #p <- crossprod(weights * X, t) / tt
    
    rho <- sqrt(sum(t * t))
    t <- t / rho
    
    d <- (w - theta * d) / rho
    
    T[, a] <- t
    W[, a] <- w
    #[, a] <- p
    #TT[a] <- tt
    
    B[a - 1, 2] <- theta
    B[a, 1] <- rho
    
    beta[, a] <- beta[, a - 1] + sum(t * y) * d
   
    }
  
  #T <-  T %*% diag(B[, 1])
  #TT <- B[, 1]^2 / n
  ## = TT <- colSums(weights * T * T)
    
  list(T = T, P = P, W = W, B = B, TT = TT, beta = beta,
    xmeans = xmeans, ymeans = ymeans, weights = weights)

  }
