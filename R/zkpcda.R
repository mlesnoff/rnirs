.kpcda <- function(Xr, Yr, Xu, Yu = NULL, ncomp, da = dalm, 
                   kern = kpol, ...) {
  
  Xr <- .matrix(Xr)
  zdim <- dim(Xr)
  n <- zdim[1]
  p <- zdim[2]
  
  Xu <- .matrix(Xu)
  m <- dim(Xu)[1]

  dots <- list(...)
  namdot <- names(dots)
  
  weights <- dots$weights
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- dots$weights
  
  z <- namdot[namdot %in% names(formals(kern))]
  if(length(z) > 0) dots.kern <- dots[z] else dots.kern <- NULL
  
  z <- namdot[namdot %in% names(formals(da))]
  if(length(z) > 0) dots.da <- dots[z] else dots.da <- NULL
  
  nclas <- length(unique(Yr))
  
  if(nclas == 1) {
    fm <- pca(Xr, Xu, ncomp = ncomp)
    }
  else{
    fm <- do.call(kpca, c(list(Xr = Xr, Xu = Xu, ncomp = ncomp, 
                               kern = kern, weights = weights), dots.kern))
    }
  
  r <- y <- fit <- vector("list", ncomp)
  for(a in 1:ncomp) {
    
    zfm <- do.call(
      da, 
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

  list(y = y, fit = fit, r = r,
    Tr = fm$Tr, Tu = fm$Tu, eig = fm$eig, sv = fm$sv,
    weights = fm$weights, explvarx = fm$explvarx, 
    T.ortho = fm$T.ortho, ni = ni)

  }



