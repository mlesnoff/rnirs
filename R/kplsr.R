kplsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...) { 
  
  namkern <- as.character(substitute(kern))
  
  dots <- list(...)
  
  if(namkern == "kpol") {
    if(is.null(dots$degree)) dots$degree <- 1
    if(is.null(dots$scale)) dots$scale <- 1
    if(is.null(dots$offset)) dots$offset <- 0
    kpar <- list(degree =  dots$degree, scale =  dots$scale, offset =  dots$offset)
    }

  if(namkern == "krbf") {
    if(is.null(dots$sigma)) dots$sigma <- 1
    kpar <- list(sigma =  dots$sigma)
    }
  
  if(namkern == "ktanh") {
    if(is.null(dots$scale)) dots$scale <- 1
    if(is.null(dots$offset)) dots$offset <- 0
    kpar <- list(scale =  dots$scale, offset =  dots$offset)
    }
  kpar <- lapply(kpar, FUN = function(x) sort(unique(x)))

  kpar <- expand.grid(kpar)
  npar <- ncol(kpar)
  nampar <- names(kpar)
  
  r <- fit <- y <- vector(mode = "list", length = npar)
  
  if(print)
    cat(paste("\n Kernel parameters: ", namkern, "\n", sep = ""))

  for(i in 1:nrow(kpar)) {
    
    if(print)
      print(kpar[i, ])
    
    zfm <- switch(namkern,
                  
      kpol = .kplsr(Xr, Yr, Xu, Yu, ncomp, kern = kpol, weights, 
                 degree = kpar[i, "degree"], scale = kpar[i, "scale"], offset = kpar[i, "offset"]),
      
      krbf = .kplsr(Xr, Yr, Xu, Yu, ncomp, kern = krbf, weights, 
                 sigma = kpar[i, "sigma"]),

      ktanh = .kplsr(Xr, Yr, Xu, Yu, ncomp, kern = ktanh, weights, 
                 scale = kpar[i, "scale"], offset = kpar[i, "offset"])
      
      )
    
    z <- dim(zfm$y)[1] 
    dat <- data.frame(matrix(rep(unlist(kpar[i, ]), z), ncol = npar, byrow = TRUE))
    names(dat) <- names(kpar)
    
    y[[i]] <- cbind(dat, zfm$y)
    fit[[i]] <- cbind(dat, zfm$fit)
    r[[i]] <- cbind(dat, zfm$r)    
    
    }
  
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))  
    
  list(y = y, fit = fit, r = r)

  }

    
    
    
