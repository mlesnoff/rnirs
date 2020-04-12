covsel <- function(X, Y, nvar = NULL, scaly = TRUE, weights = rep(1, nrow(X))) {
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "x")
  q <- ncol(Y)
  
  if(is.null(nvar)) nvar <- p
  
  d <- weights / sum(weights)
  D <- diag(d)
  
  xmeans <- .xmeans(X, weights = d)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  ymeans <- .xmeans(Y, weights = d)
  Y <- scale(Y, center = ymeans, scale = FALSE)
  
  if(scaly)
    Y <- scale(Y, center = FALSE, scale = sqrt(colSums(d * Y * Y)))
  
  xsstot <- sum(colSums(d * X * X))
  ysstot <- sum(colSums(d * Y * Y))
  
  yss <- xss <- selvar <- vector(length = nvar)
  for(i in 1:nvar) {
    
    z <- rowSums(crossprod(d * X, Y)^2)
    selvar[i] <- which(z == max(z))
    
    u <- X[, selvar[i], drop = FALSE]
    
    Pr <- tcrossprod(u) %*% D / sum(d * u * u)
    # Same as:
    #Pr <- u %*% solve(t(u) %*% D %*% u) %*% t(u) %*% D
    #Pr <- u %*% t(u) %*% D / sum(d * u * u)
    #Pr <- crossprod(u %*% t(u), D) / sum(d * u * u)

    X <- X - Pr %*% X       # The deflated X is a centered matrix (metric D)
    Y <- Y - Pr %*% Y       # The deflated Y is a centered matrix (metric D)
    
    xss[i] <- sum(colSums(d * X * X))
    yss[i] <- sum(colSums(d * Y * Y))
  
    }

  cumpvarx <- 1 - xss / xsstot
  cumpvary <- 1 - yss / ysstot
  
  sel <- data.frame(sel = selvar, cumpvarx = cumpvarx, cumpvary = cumpvary)
  
  list(sel = sel, weights = d)

}

