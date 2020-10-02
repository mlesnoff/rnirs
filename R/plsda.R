plsda <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, da = dalm, ...) {
  
  dots <- list(...)
  namdot <- names(dots)
  
  z <- namdot[namdot %in% names(formals(algo))]
  if(length(z) > 0) dots.algo <- dots[z] else dots.algo <- NULL
  
  z <- namdot[namdot %in% names(formals(da))]
  if(length(z) > 0) dots.da <- dots[z] else dots.da <- NULL
  
  nclas <- length(unique(Yr))
  Ydummy <- dummy(Yr)
  
  if(nclas == 1) {
    fm <- pca(Xr, ncomp = ncomp)
    fm$T <- fm$Tr
    fm$ymeans <- .xmean(Ydummy, weights = fm$weights)
    }
  else
    fm <- do.call(algo, c(list(X = Xr, Y = Ydummy, ncomp = ncomp), dots.algo)) 
  
  Tu <- .projscor(fm, .matrix(Xu))
  m <- dim(Tu)[1]
  
  zfm <- r <- y <- fit <- vector("list", ncomp)
  for(a in 1:ncomp) {
    
    zfm[[a]] <- do.call(
      da, 
      c(list(Xr = fm$T[, 1:a, drop = FALSE], Yr = Yr,
        Xu = Tu[, 1:a, drop = FALSE], Yu = Yu), dots.da)
      )
    
    y[[a]] <- zfm[[a]]$y
    fit[[a]] <- zfm[[a]]$fit
    r[[a]] <- zfm[[a]]$r
    
    }
  
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  dat <- data.frame(ncomp = sort(rep(1:ncomp, m)))
  
  y <- data.frame(dat, y, stringsAsFactors = FALSE)
  fit <- data.frame(dat, fit, stringsAsFactors = FALSE)
  r <- data.frame(dat, r, stringsAsFactors = FALSE)
  
  list(y = y, fit = fit, r = r,
    Tr = fm$T, Tu = Tu, P = fm$P, W = fm$W, R = fm$R, C = fm$C, TT = fm$TT,
    xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights,
    T.ortho = fm$T.ortho, da.models = zfm, ni = zfm$ni)

  }


