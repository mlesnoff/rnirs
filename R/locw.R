locw <- function(
  Xr = NULL, Yr,
  Xu = NULL, Yu = NULL,
  listnn,
  listw = NULL,
  fun,
  stor = TRUE,
  print = TRUE,
  ...) {
  
  .fun <- match.fun(fun)
  dots <- list(...)

  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
  n <- nrow(Yr)
  q <- ncol(Yr)
  colnam.Yr <- colnames(Yr)
  
  m <- length(listnn)
  
  if(is.null(Yu)) Yu <- matrix(nrow = m, ncol = q)
    else Yu <- .matrix(Yu, row = FALSE, prefix.colnam = "y")
  
  if(!is.null(Xr)) Xr <- .matrix(Xr) else Xr <- Yr
  if(!is.null(Xu)) Xu <- .matrix(Xu) else Xu <- Yu
  rownam.Xu <- row.names(Xu)
  
  weighted <- !is.null(listw)
  fm <- y <- r <- fit <- vector("list", length = m)
  names(fm) <- 1:m 
  for(i in 1:m) {
    
    if(print)
      if(m <= 220) cat(i, "") else if(i %in% seq(1, m, by = 5)) cat(i, "")
    
    ind <- listnn[[i]]
    
    zXr <- Xr[ind, , drop = FALSE]
    zYr <- Yr[ind, , drop = FALSE]
      
    zXu <- Xu[i, , drop = FALSE]
    zYu <- Yu[i, , drop = FALSE]
    
    if(weighted) dots$weights <- listw[[i]]
    param <- list(Xr = zXr, Yr = zYr, Xu = zXu, Yu = zYu)
    param <- c(param, dots)
    zfm <- do.call(.fun, param)
    
    zy <- zfm$y
    zfit <- zfm$fit
    zr <- zfm$r
    
    n <- nrow(zy)
    zr$rownum <- zfit$rownum <- zy$rownum <- rep(i, n)
    zr$rownam <- zfit$rownam <- zy$rownam <- rep(rownam.Xu[i], n)
    
    k <-  rep(length(ind), n)
    zy <- data.frame(k = k, zy)
    zfit <- data.frame(k = k, zfit)
    zr <- data.frame(k = k, zr)
    
    y[[i]] <- zy
    fit[[i]] <- zfit
    r[[i]] <- zr
    
    if(stor) fm[[i]] <- zfm
    
    }

  if(print) cat("\n\n")
  
  if(!stor) fm <- NULL

  .f <- function(x) {
    z <- lapply(x, function(x) data.frame(x))
    z <- setDF(rbindlist(z))
    }
  y <- .f(y)
  fit <- .f(fit)
  r <- .f(r)
  
  gc()
  
  list(y = y, fit = fit, r = r, fm = fm)
  
  }
  
  