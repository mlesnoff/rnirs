xfit <- function(T, P, xmeans = NULL) {

  T <- .matrix(T)
  P <- .matrix(P)

  X <- tcrossprod(T, P)
  
  if(!is.null(xmeans))
    X <- scale(X, center = -xmeans, scale = FALSE)
  
  attributes(X)["scaled:center"] <- NULL

  X
  
  }
