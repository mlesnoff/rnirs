pca.eigenw <- function(X, ncomp, weights = rep(1, nrow(X))) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)
  
  d <- weights / sum(weights)
  
  xmeans <- crossprod(d, X)
  X <- scale(X, center = xmeans, scale = FALSE)

  z <- eigen(crossprod(sqrt(d) * X))
  
  sv <- sqrt(z$values[1:ncomp])
  
  P <- z$vectors[, 1:ncomp, drop = FALSE]

  T <- X %*% P
  
  xss <- colSums(T * T)
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  colnames(T) <- colnames(P) <- nam
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, xmeans = xmeans, weights = d)

}




