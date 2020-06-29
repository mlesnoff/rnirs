lmridge <- function(Xr, Yr, Xu, Yu = NULL, lambda = 0, algo = NULL,
  weights = NULL, ...) {
  
  X <- .matrix(Xr)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  Y <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  q <- dim(Y)[2]
  colnam.Y <- colnames(Y)
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)  

  if(is.null(algo))
    if(n < 2000 & (n < p))
      algo <- pca.eigenk
    else
      algo <- pca.eigen
  
  ncomp <- min(n, p)
  fm <- algo(X, ncomp, weights = weights, ...)
  
  fm$ymeans <- .xmean(Y, weights = fm$weights)
  
  Tu <- .projscor(fm, .matrix(Xu))
  
  m <- dim(Tu)[1]
  rownam.Xu <- row.names(Tu)
  
  if(is.null(Yu)) 
    Yu <- matrix(nrow = m, ncol = q)
  else {
    if(q == 1)
      row <- FALSE 
    else 
      row <- TRUE
    Yu <- .matrix(Yu, row = row)
    }
  
  Ymeans <- matrix(rep(fm$ymeans, m), nrow = m, byrow = TRUE)
  lambda <- sort(lambda)
  nlambda <- length(lambda)
  r <- fit <- y <- array(dim = c(m, nlambda, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans
  tr <- vector(length = nlambda)
  beta <- vector(length = nlambda, mode = "list")
  
  Y <- .center(Y, fm$ymeans)
  
  z <- 1 / fm$eig * t(fm$T) %*% (fm$weights * Y)
  #z <- coef(lm(Y ~ fm$T - 1, weights = fm$weights))
  zbeta <- matrix(z, nrow = ncomp, ncol = q)
  
  for(i in 1:nlambda) {
    
    ztr <- fm$eig / (fm$eig + lambda[[i]] / n)
    if(lambda[[i]] == 0)
      ztr[fm$eig < 1e-10] <- 0
  
    beta[[i]] <- ztr * zbeta

    y[, i, ] <- Yu
    fit[, i, ] <- Ymeans + Tu %*% beta[[i]]
    tr[i] <- sum(ztr)
    
    }
  
  y <- matrix(c(y), nrow = m * nlambda, ncol = q, byrow = FALSE)
  fit <- matrix(c(fit), nrow = m * nlambda, ncol = q, byrow = FALSE)
  r <- y - fit

  dat <- data.frame(
    lambda = sort(rep(lambda, m)),
    rownum = rep(1:m, nlambda),
    rownam = rep(rownam.Xu, nlambda)
    )
  
  y <- cbind(dat, y)
  fit <- cbind(dat, fit)
  r <- cbind(dat, r)
  
  zq <- ncol(y)
  u <- (zq - q + 1):zq
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Y

  list(y = y, fit = fit, r = r,
    Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, eig = fm$eig,
    xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights,
    beta = beta, tr = tr)

  }

