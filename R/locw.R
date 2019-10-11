locw <- function(
  Xr = NULL, Yr,
  Xu = NULL, Yu = NULL,
  listnn,
  listw = NULL,
  fun,
  print = TRUE,
  ...
  ) {
  
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
    
    if(weighted) dots$weights <- listw[[i]]
    param <- c(
      list(
        Xr = Xr[ind, , drop = FALSE], 
        Yr = Yr[ind, , drop = FALSE], 
        Xu = Xu[i, , drop = FALSE], 
        Yu = Yu[i, , drop = FALSE]), 
      dots
      )
    fm[[i]] <- do.call(.fun, param)
    
    nr <- nrow(fm[[i]]$y)
    fm[[i]]$r$rownum <- fm[[i]]$fit$rownum <- fm[[i]]$y$rownum <- rep(i, nr)
    fm[[i]]$r$rownam <- fm[[i]]$fit$rownam <- fm[[i]]$y$rownam <- rep(rownam.Xu[i], nr)
    
    k <-  rep(length(ind), nr)

    y[[i]] <- data.frame(k = k, fm[[i]]$y)
    fit[[i]] <- data.frame(k = k, fm[[i]]$fit)
    r[[i]] <- data.frame(k = k, fm[[i]]$r)
    
    fm[[i]][c("y", "fit", "r")] <- NULL  
    
    }

  if(print) cat("\n\n")

  .f <- function(x) {
    x <- lapply(x, function(x) data.frame(x))
    x <- setDF(rbindlist(x))
    }
  y <- .f(y)
  fit <- .f(fit)
  r <- .f(r)
  
  rm(list = c("param"))
  gc()
  
  list(y = y, fit = fit, r = r, fm = fm)
  
  }
  
  