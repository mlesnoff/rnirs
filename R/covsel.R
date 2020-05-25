covsel <- function(X, Y, nvar = NULL, scaly = TRUE, weights = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "x")
  q <- dim(Y)[2]
  
  if(is.null(nvar)) nvar <- p
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)
  
  ymeans <- .xmean(Y, weights = weights)
  Y <- .center(Y, ymeans)
  
  if(scaly)
    Y <- .scale(Y, rep(0, q), sqrt(colSums(weights * Y * Y)))
  
  xsstot <- sum(weights * X * X)
  ysstot <- sum(weights * Y * Y)
  
  yss <- xss <- selvar <- vector(length = nvar)
  for(i in 1:nvar) {
    
    z <- rowSums(crossprod(weights * X, Y)^2)
    selvar[i] <- which(z == max(z))
    
    u <- X[, selvar[i], drop = FALSE]
    
    Pr <- tcrossprod(u) %*% diag(weights) / sum(weights * u * u)
    # Same as:
    #Pr <- u %*% solve(t(u) %*% D %*% u) %*% t(u) %*% D
    #Pr <- u %*% t(u) %*% D / sum(d * u * u)
    #Pr <- crossprod(u %*% t(u), D) / sum(d * u * u)

    X <- X - Pr %*% X       # The deflated X is a centered matrix (metric D)
    Y <- Y - Pr %*% Y       # The deflated Y is a centered matrix (metric D)
    
    xss[i] <- sum(weights * X * X)
    yss[i] <- sum(weights * Y * Y)
  
    }

  cumpvarx <- 1 - xss / xsstot
  cumpvary <- 1 - yss / ysstot
  
  sel <- data.frame(sel = selvar, cumpvarx = cumpvarx, cumpvary = cumpvary)
  
  list(sel = sel, weights = weights)

}

