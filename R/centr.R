centr <- function(X, Y = NULL, FUN = mean, ...) {
  
  X <- .matrix(X)
  n <- nrow(X)
  
  if(is.null(Y)) Y <- rep(1, nrow(X))

  Y <- as.factor(Y)
  lev <- levels(Y)
  zy <- as.numeric(Y)
  
  ni <- tabulate(Y)

  centers <- as.matrix(aggregate(X ~ Y, FUN = FUN, ...)[, -1])
  rownames(centers) <- lev
  
  Xc <- X
  for(i in 1:n) Xc[i, ] <- centers[zy[i], ]

  list(centers = centers, ni = ni, Xc = Xc)

  }
