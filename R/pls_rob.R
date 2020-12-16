pls_rob <- function(X, Y, ncomp, ncompw = 10, alpha = .30,
                    typcut = c("param", "mad"), weights = NULL, ...) {
  
  typcut <- match.arg(typcut)
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)  
  
  ncompw <- min(ncompw, n, p)
  
  fm <- pca.rob(X, ncompw, typcut = typcut, weights = weights, ...)
  #fm <- pca.sph(X, min(ncompw, n, p), ...)
  r <- out.sdod(fm, X, typcut = typcut)
  wx <- .talworth(r, quantile(r, 1 - alpha))

  fm <- pls.kernel(X, Y, ncomp = ncompw, weights = weights * wx)
  r <- .resid.pls(fm, Y)$r
  r <- (r - median(r)) / mad(r) 
  cutoff <- switch(
    typcut, 
    param = qnorm(p = .975),
    mad = 2.5
    )  
  wy <- .talworth(r, cutoff)
  
  w <- wx * wy

  fm <- pls.kernel(X, Y, ncomp = ncomp, weights = weights * w)
  
  fm

  }


