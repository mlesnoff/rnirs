plsda <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, da = dalm, ...) {
  
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
    param <- list(Xr = Xr, Yr = Yrdummy, Xu = Xu, ncomp = ncomp, algo = algo)
    param <- c(param, dots.pls.algo)
    fm <- do.call(pls, param)  
    }
  
  Tr <- fm$Tr
  Tu <- fm$Tu
  m <- nrow(Tu)
  
  P <- fm$P
  C <- fm$C
  R <- fm$R
  xmeans <- fm$xmeans
  ymeans <- fm$ymeans
  
  d <- fm$weights
  
  r <- y <- fit <- vector("list", ncomp)
  for(a in 1:ncomp) {
    
    param <- list(Xr = Tr[, 1:a, drop = FALSE], Yr = Yr, Xu = Tu[, 1:a, drop = FALSE], Yu = Yu)
    param <- c(param, dots.da)
    zfm <- do.call(.da, param)
    
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
  
  list(y = y, fit = fit, r = r, ni = ni,
    Tr = Tr, Tu = Tu, P = P, C = C, R = R, xmeans = xmeans, ymeans = ymeans,
    Xr = Xr, Xu = Xu, weights = d)

  }


