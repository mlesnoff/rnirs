dkplsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...) { 
  
  
  namkern <- as.character(substitute(kern))
  
  dots <- list(...)
  
  z <- formals(kern)
  nam <- names(z)
  nam <- nam[-match(c("X", "Y"), nam)]
  z <- z[nam]
  ndots <- length(dots)
  if(ndots > 0)
    for(i in 1:ndots)
      if(names(dots[i]) %in% nam)
        z[[names(dots[i])]] <- dots[[i]]
  listkpar <- lapply(z, FUN = function(x) sort(unique(x)))
  
  kpar <- expand.grid(listkpar)
  npar <- ncol(kpar)
  nampar <- names(kpar)
  
  fm <- r <- fit <- y <- fm <- vector(mode = "list", length = npar)
  
  if(print)
    cat(paste("\n Kernel parameters: ", namkern, "\n", sep = ""))

  for(i in 1:nrow(kpar)) {
    
    zkpar <- kpar[i, , drop = FALSE]
    
    if(print)
      print(zkpar)

    res <- kgram(Xr = Xr, Xu = Xu, kern = kern, sigma = 1)
    
    res <- do.call(
      kgram, 
      c(list(Xr = Xr, Xu = Xu, kern = kern), 
        zkpar)
      )
    
    fm[[i]] <- plsr(res$Kr, Yr, res$Ku, Yu, ncomp, algo = pls.kernel, weights = weights)
    
    z <- dim(fm[[i]]$y)[1] 
    dat <- data.frame(matrix(rep(unlist(kpar[i, ]), z), ncol = npar, byrow = TRUE))
    names(dat) <- names(kpar)
    
    y[[i]] <- cbind(dat, fm[[i]]$y)
    fit[[i]] <- cbind(dat, fm[[i]]$fit)
    r[[i]] <- cbind(dat, fm[[i]]$r)

    }
  
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))  
    
  list(y = y, fit = fit, r = r, fm = fm)

  }

    
    
    
