matdis <- function(X, diss = c("euclidean", "mahalanobis", "correlation")) {
  
  diss <- match.arg(diss)

  X <- .matrix(X, row = FALSE)
  rownam <- row.names(X)

  if(diss == "euclidean") D <- as.matrix(dist(X))
    
  if(diss == "mahalanobis") {
    
    V <- cov(X)
    U <- chol(V)
    X <- X %*% solve(U) 
    
    D <- as.matrix(dist(X))
    
    }
  
  if(diss == "correlation") {
    
    X <- t(X)
    X <- t(cor(X, X))
    
    D <- sqrt(.5 * (1 - X))    # z <- seq(-1, 1, .1) ; plot(z, sqrt(.5 * (1 - z)))
    
    }

  row.names(D) <- colnames(D) <- rownam
  
  D
  
  }
