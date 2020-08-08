dkplsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, 
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

  kpar <- expand.grid(kpar)
  npar <- ncol(kpar)
  nampar <- names(kpar)
  
  r <- fit <- y <- vector(mode = "list", length = npar)
  
  if(print)
    cat(paste("\n Kernel parameters: ", namkern, "\n", sep = ""))

  for(i in 1:nrow(kpar)) {
    
    if(print)
      print(kpar[i, ])
    
    res <- switch(namkern,
                  
      kpol = kgram(Xr, Xu, kern = kpol, 
                 degree = kpar[i, "degree"], scale = kpar[i, "scale"], offset = kpar[i, "offset"]),
      
      krbf = kgram(Xr, Xu, kern = krbf, 
                 sigma = kpar[i, "sigma"]),

      ktanh = kgram(Xr, Xu, kern = ktanh, 
                 scale = kpar[i, "scale"], offset = kpar[i, "offset"])
      
      )
    
    zfm <- plsr(res$Kr, Yr, res$Ku, Yu, ncomp, algo = pls.kernel, weights = weights)
    
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

    
    
    