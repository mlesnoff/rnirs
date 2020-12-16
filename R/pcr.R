pcr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = NULL, ...) {
  
  Xr <- .matrix(Xr)
  zdim <- dim(Xr)
  n <- zdim[1]
  p <- zdim[2]
  
  Xu <- .matrix(Xu)
  m <- dim(Xu)[1]
  rownam.Xu <- row.names(Xu)

  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  q <- dim(Yr)[2]
  colnam.Y <- colnames(Yr)

  if(is.null(algo))
    if(n < p)
      algo <- pca_eigenk
    else
      algo <- pca_eigen
  
  fm <- algo(Xr, ncomp, ...)
  if(!fm$T.ortho)
    stop("This function is not implemented for algorithms providing
      non orthogonal scores.") 
  
  Tu <- .projscor(fm, .matrix(Xu))
  
  if(is.null(Yu)) 
    Yu <- matrix(nrow = m, ncol = q)
  else {
    if(q == 1)
      row <- FALSE 
    else 
      row <- TRUE
    Yu <- .matrix(Yu, row = row)
    }
  
  tt <- colSums(fm$weights * fm$T * fm$T)
  beta <- 1 / tt * crossprod(fm$T, fm$weights * Yr)
  
  ymeans <- .xmean(Yr, fm$weights)
  Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
  r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans
  
  for(a in 1:ncomp) {
    
    y[, a + 1, ] <- Yu
    fit[, a + 1, ] <- Ymeans + Tu[, 1:a, drop = FALSE] %*% beta[1:a, , drop = FALSE]
    
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
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Y

  list(y = y, fit = fit, r = r,
    Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, beta = beta, eig = fm$eig,
    xmeans = fm$xmeans, ymeans = ymeans, weights = fm$weights,
    T.ortho = fm$T.ortho)

  }

