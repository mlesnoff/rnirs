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
  
  if(!is.null(Xr)) Xr <- .matrix(Xr, prefix.colnam = "x") else Xr <- Yr
  if(!is.null(Xu)) Xu <- .matrix(Xu, prefix.colnam = "x") else Xu <- Yu
  rownam.Xu <- row.names(Xu)
  
  weighted <- !is.null(listw)

  res <- lapply(
    
    1:m, function(i) {
  
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
      y <- data.table(k = k, zy)
      fit <- data.frame(k = k, zfit)
      r <- data.frame(k = k, zr)
      
      if(!stor) zfm <- NULL
      fm <- zfm
      
      return(list(y = y, fit = fit, r = r, fm = fm))
      
      }
    
    )
  
  if(print) cat("\n\n")
  
  .f <- function(nam) {
    z <- lapply(1:length(res), function(i) {res[[i]][nam]})
    z <- unlist(z, recursive = FALSE)
    z <- setDF(rbindlist(z))
    }
  y <- .f("y")
  fit <- .f("fit")
  r <- .f("r")
  
  z <- lapply(1:length(res), function(i) {res[[i]]["fm"]})
  fm <- unlist(z, recursive = FALSE)
  names(fm) <- 1:m
  
  if(!stor) fm <- NULL

  gc()
  
  list(y = y, fit = fit, r = r, fm = fm)
  
  }
  
  