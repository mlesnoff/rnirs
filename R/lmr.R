lmr <- function(Xr, Yr, Xu, Yu = NULL, weights = NULL) {
  
  Xr <- .matrix(Xr, prefix.colnam = "x")
  n <- nrow(Xr)
  p <- ncol(Xr)
  
  Xu <- .matrix(Xu, prefix.colnam = "x")
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)

  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  q <- ncol(Yr)
  colnam.Yu <- colnames(Yr)
  
  if(is.null(Yu)) Yu <- matrix(nrow = m, ncol = q)
    else Yu <- .matrix(Yu, row = FALSE, prefix.colnam = "y")
  
  if(is.null(weights))
    weights <- rep(1, n)
  d <- weights / sum(weights)
  
  fm <- lm(Yr ~ Xr, weights = d)
  
  y <- Yu
  fit <- cbind(rep(1, m), Xu) %*% fm$coef
  r <- y - fit
  
  dat <- data.frame(
    rownum = 1:m, 
    rownam = rownam.Xu
    )
  
  y <- cbind(dat, y)
  fit <- cbind(dat, fit)
  r <- cbind(dat, r)
  
  zq <- ncol(y)
  u <- (zq - q + 1):zq
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Yu
  
  list(y = y, fit = fit, r = r, fm = fm)
  
  }