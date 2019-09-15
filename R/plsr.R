plsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, ...) {
  
  .pls.algo <- match.fun(FUN = algo)
  
  Xr <- .matrix(Xr, prefix.colnam = "x")
  n <- nrow(Xr)
  p <- ncol(Xr)

  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  q <- ncol(Yr)
  colnam.Yr <- colnames(Yr)

  Xu <- .matrix(Xu)
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)

  if(is.null(Yu)) Yu <- matrix(nrow = m, ncol = q)
    else {
      if(q == 1) row <- FALSE else row <- TRUE
      Yu <- .matrix(Yu, row = row, prefix.colnam = "y")
      }
  
  fm <- .pls.algo(Xr, Yr, ncomp, ...)
  
  Tr <- fm$T
  Tu <- projscor(Xu, fm)
  
  P <- fm$P
  C <- fm$C
  R <- fm$R
  xmeans <- fm$xmeans
  ymeans <- fm$ymeans
  
  d <- fm$weights
  
  Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
  
  Beta <- t(C)
  
  r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans

  for(a in 1:ncomp) {
    y[, a + 1, ] <- Yu
    fit[, a + 1, ] <- Ymeans + Tu[, 1:a, drop = FALSE] %*% Beta[1:a, , drop = FALSE]
    }
  
  y <- matrix(c(y), nrow = m * (ncomp + 1), ncol = q, byrow = FALSE)
  fit <- matrix(c(fit), nrow = m * (ncomp + 1), ncol = q, byrow = FALSE)
  r <- y - fit

  dat <- data.frame(
    ncomp = sort(rep(0:ncomp, m)),
    rownum = rep(1:m, ncomp + 1),
    rownam = rep(rownam.Xu, ncomp + 1)
    )
  
  y <- cbind(dat, y)
  fit <- cbind(dat, fit)
  r <- cbind(dat, r)
  
  zq <- ncol(y)
  u <- (zq - q + 1):zq
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Yr

  list(y = y, fit = fit, r = r,
    Tr = Tr, Tu = Tu, P = P, C = C, R = R, xmeans = xmeans, ymeans = ymeans,
    Xr = Xr, Xu = Xu, weights = d)
  
  }


