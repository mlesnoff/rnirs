pcr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pca.svd, ...) {
  
  .pca.algo <- match.fun(FUN = algo)
  
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
  
  fm <- .pca.algo(Xr, ncomp, ...)
  
  Tr <- fm$T
  Tu <- projscor(Xu, fm)
  
  P <- fm$P
  R <- fm$R
  sv <- fm$sv 
  xmeans <- fm$xmeans
  
  d <- fm$weights
  
  ### FOR INCREASING SPEED
  if(("weights" %in% names(list(...)))) {
    ymeans <- crossprod(d, Yr)
    Beta <- 1 / sv^2 * crossprod(Tr, d * Yr)
     }
  else {
    ymeans <- colMeans(Yr)
    Beta <- 1 / sv^2 * crossprod(Tr, Yr)
    }  
  ### END
  
  C <- t(Beta)
  
  Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
  
  r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans

  lapply(
    1:ncomp, function(a) {
      
      y[, a + 1, ] <<- Yu
      fit[, a + 1, ] <<- Ymeans + Tu[, 1:a, drop = FALSE] %*% Beta[1:a, , drop = FALSE]
      
      }
    )
    
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


