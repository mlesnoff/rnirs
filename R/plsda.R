plsda <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, da = dalm, 
  stor = FALSE, ...) {
  
  .pls.algo <- match.fun(FUN = algo)
  
  .da <- match.fun(FUN = da)
  
  dots <- list(...)
  namdot <- names(dots)
  
  z <- namdot[namdot %in% names(formals(.pls.algo))]
  if(length(z) > 0) dots.pls.algo <- dots[z] else dots.pls.algo <- NULL
  
  z <- namdot[namdot %in% names(formals(.da))]
  if(length(z) > 0) dots.da <- dots[z] else dots.da <- NULL
  
  nclas <- length(unique(Yr))
  
  if(nclas == 1)
    fm <- pca(Xr, Xu, ncomp = ncomp)
  else {
    Yrdummy <- dummy(Yr)
    fm <- do.call(
      pls, 
      c(list(Xr = Xr, Yr = Yrdummy, Xu = Xu, ncomp = ncomp,
        algo = algo), dots.pls.algo)
      ) 
    }
  
  m <- nrow(fm$Tu)
  
  r <- y <- fit <- vector("list", ncomp)
  for(a in 1:ncomp) {
    
    zfm <- do.call(
      .da, 
      c(list(Xr = fm$Tr[, 1:a, drop = FALSE], Yr = Yr,
        Xu = fm$Tu[, 1:a, drop = FALSE], Yu = Yu), dots.da)
      )
    
    y[[a]] <- zfm$y
    fit[[a]] <- zfm$fit
    r[[a]] <- zfm$r
    
    }
  ni <- zfm$ni
  
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  dat <- data.frame(ncomp = sort(rep(1:ncomp, m)))
  
  y <- data.frame(dat, y, stringsAsFactors = FALSE)
  fit <- data.frame(dat, fit, stringsAsFactors = FALSE)
  r <- data.frame(dat, r, stringsAsFactors = FALSE)
  
  if(!stor) fm <- NULL
  if(stor) {
    z <- sdod(Xr, Xu, fm)
    fm$sd <- z$sdu
    fm$od <- z$odu
    }
  
  list(y = y, fit = fit, r = r, ni = ni, fm = fm)

  }


