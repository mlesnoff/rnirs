matdis <- function(X, diss = c("euclidean", "mahalanobis", "correlation"),
  weights = NULL) {
  
  diss <- match.arg(diss)

  X <- .matrix(X, row = FALSE)
  n <- dim(X)[1]
  rownam <- row.names(X)

  if(diss == "euclidean") D <- as.matrix(dist(X))
    
  if(diss == "mahalanobis") {
  
    if(is.null(weights))
      S <- cov(X) * (n - 1) / n
    else 
      S <- .xcov(X, weights = weights)

    U <- chol(S)
    X <- X %*% solve(U) 
    
    D <- as.matrix(dist(X))
    
    }
  
  if(diss == "correlation") {
    
    X <- t(X)
    X <- t(cor(X, X))
    
    D <- sqrt(.5 * (1 - X))
    ## z <- seq(-1, 1, .1) ; plot(z, sqrt(.5 * (1 - z)))
    
    }

  row.names(D) <- colnames(D) <- rownam
  
  D
  
  }
