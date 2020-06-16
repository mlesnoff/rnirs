pcr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, meth = pca, ...) {
  
  fm <- meth(Xr, Xu, ncomp, ...)
  
  m <- dim(fm$Tu)[1]
  rownam.Xu <- row.names(fm$Tu)

  Y <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  q <- dim(Y)[2]
  colnam.Y <- colnames(Y)
  
  weights <- fm$weights
  ymeans <- .xmean(Y, weights = weights) 
  Y <- .center(Y, ymeans)  

  if(is.null(Yu)) 
    Yu <- matrix(nrow = m, ncol = q)
  else {
    if(q == 1) 
      row <- FALSE 
    else 
      row <- TRUE
    Yu <- .matrix(Yu, row = row)
    }

  Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
  
  r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
  y[, 1, ] <- Yu
  fit[, 1, ] <- Ymeans
  
   if(fm$T.ortho) {
     z <- coef(lm(Y ~ fm$Tr[, 1:ncomp, drop = FALSE] - 1, weights = weights))
     beta <- matrix(z, nrow = ncomp, ncol = q)
     ## Same as:
     #zT <- weights * fm$Tr
     #beta <- solve(crossprod(zT, fm$Tr)) %*% crossprod(zT, Y)
     }

  if(fm$T.ortho) {
    
    z <- coef(lm(Y ~ fm$Tr[, 1:ncomp, drop = FALSE] - 1, weights = weights))
    beta <- matrix(z, nrow = ncomp, ncol = q)
    ## Same as:
    #zT <- weights * fm$Tr
    #beta <- solve(crossprod(zT, fm$Tr)) %*% crossprod(zT, Y)
    
    }
  
  for(a in 1:ncomp) {
    
    if(!fm$T.ortho) {
      z <- coef(lm(Y ~ fm$Tr[, 1:a, drop = FALSE] - 1, weights = weights))
      beta <- matrix(z, nrow = a, ncol = q)
      }
    
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
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Y

  list(y = y, fit = fit, r = r, fm = fm)
  
  }


