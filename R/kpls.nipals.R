kpls.nipals <- function(X, Y, ncomp, kern = kpol, weights = NULL, ...) {
  
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
  
  K <- kern(X, ...)
  tK <- t(K)
  Kc <- K <- t(t(K - colSums(weights * tK)) - colSums(weights * tK)) + 
    sum(weights * t(weights * tK))

  ymeans <- .xmean(Y, weights = weights) 
  Y <- .center(Y, ymeans)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  U <- T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))           
  C <- matrix(nrow = q, ncol = ncomp, dimnames = list(colnames(Y), nam))           
  
  for(a in 1:ncomp) {
    
    if(q == 1) {
      t <- K %*% (weights * Y)
      }
    else {
      A <- K %*% (weights * tcrossprod(Y)) %*% diag(weights)
      ## Slow
      t <- .eigpow(A)$v
      ## End
      }
    
    t <- t / sqrt(sum(weights * t * t))
    
    c <- crossprod(weights * Y, t)
    
    u <- Y %*% c
    u <- u / sqrt(sum(u * u))
    
    ## Slow
    z <- diag(n) - tcrossprod(t, weights * t)
    K <- z %*% K %*% t(z)
    ## End
    
    Y <- Y - tcrossprod(t, c)
    
    T[, a] <- t
    C[, a] <- c
    U[, a] <- u

    }

  DU <- weights * U
  zR <- DU %*% solve(crossprod(T, weights * Kc) %*% DU)

  list(T = T, C = C, U = U, R = zR,
    ymeans = ymeans, weights = weights, T.ortho = TRUE)

  }
