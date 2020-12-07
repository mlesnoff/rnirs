mdi.ia <- function(X, ncomp, algo = NULL,
                   start = c("nipals", "mean"),
                   tol = .Machine$double.eps^0.5, 
                   maxit = 10000,
                   gs = TRUE,
                   print = TRUE, ...) {

  start <- match.arg(start)
  
  dots <- list(...)
  namdot <- names(dots)
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  N <- n * p
  
  ncomp <- min(ncomp, n, p)
  
  if(is.null(algo))
    if(n < p)
      algo <- pca.eigenk
    else
      algo <- pca.eigen
  
  s <- which(is.na(X))
  
  if(start == "nipals") {
    fm <- pca.nipalsna(X, ncomp, gs = gs)
    fit <- xfit(fm$T[, 1:ncomp, drop = FALSE], fm$P[, 1:ncomp, drop = FALSE], fm$xmeans)[s]
    }
  
  if(start == "mean") {
    xmeans <- matrixStats::colMeans2(X, na.rm = TRUE)     ## to do: Add weight
    fit <- matrix(rep(xmeans, n), nrow = n, byrow = TRUE)[s]
    }

  X[s] <- fit
  
  iter <- 1 ; ztol <- 1
  while(ztol[iter] > tol & iter < maxit) {
    
    iter <- iter + 1
    if(print)
      cat(iter, " ") 
    
    fit <- X[s]
    
    fm <- algo(X, ncomp, ...)
    zfit <- xfit(fm$T[, 1:ncomp, drop = FALSE], fm$P[, 1:ncomp, drop = FALSE], fm$xmeans)[s]
    X[s] <- zfit
    
    ztol[iter] <- .xnorm(fit - zfit) / .xnorm(fit)
    
    }
  
  if(print & iter > 1)
    cat("\n\n")
  
  list(X = X, T = fm$T, P = fm$P, xmeans = fm$xmeans, 
       fit = X[s], s = s, niter = iter, tol = ztol) 

  }
