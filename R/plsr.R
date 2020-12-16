plsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 algo = NULL, ...) {
  
  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  q <- dim(Yr)[2]
  colnam.Y <- colnames(Yr)
  
  if(is.null(algo))
    algo <- pls_kernel

  fm <- algo(Xr, Yr, ncomp, ...)
  if(!fm$T.ortho)
    stop("This function is not implemented for algorithms providing
      non orthogonal scores.") 

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
  r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans
  
  beta <- t(fm$C)
  
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
    Tr = fm$T, Tu = Tu, P = fm$P, W = fm$W, R = fm$R, C = fm$C, TT = fm$TT,
    xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights,
    T.ortho = fm$T.ortho)

  }



