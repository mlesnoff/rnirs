plsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, 
  stor = FALSE, ...) {
  
  .pls.algo <- match.fun(FUN = algo)

  fm <- pls(Xr, Yr, Xu, ncomp, algo, ...)
  
  m <- nrow(fm$Tu)
  rownam.Xu <- row.names(fm$Tu)
  q <- length(fm$ymeans)
  colnam.Yu <- names(fm$ymeans)
  
  if(is.null(Yu)) 
    Yu <- matrix(nrow = m, ncol = q)
  else {
    if(q == 1) row <- FALSE else row <- TRUE
    Yu <- .matrix(Yu, row = row)
    }

  Ymeans <- matrix(rep(fm$ymeans, m), nrow = m, byrow = TRUE)
  
  beta <- t(fm$C)
  
  r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans

  for(a in 1:ncomp) {
    y[, a + 1, ] <- Yu
    fit[, a + 1, ] <- Ymeans + fm$Tu[, 1:a, drop = FALSE] %*% beta[1:a, , drop = FALSE]
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
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Yu
  
  if(!stor) fm <- NULL
  if(stor) {
    
    fm$bcoef <- bcoef(fm)   
    
    z <- sdod(Xr, Xu, fm)
    fm$sd <- z$sdu
    fm$od <- z$odu
    
    }

  list(y = y, fit = fit, r = r, fm = fm)

  }


