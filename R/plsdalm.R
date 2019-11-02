plsdalm <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, 
  stor = FALSE, ...) {

  dots <- list(...)
  namdot <- names(dots)
  
  z <- namdot[namdot %in% names(formals(algo))]
  if(length(z) > 0) dots.algo <- dots[z] else dots.algo <- NULL

  colnam.Yr <- colnames(Yr)
  if(is.null(colnam.Yr)) colnam.Yr <- "y1"
    
  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)

  lev <- levels(Yr)
  namclas <- as.character(lev)
  
  Xu <- .matrix(Xu)
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)

  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fm <- pca(Xr, Xu, ncomp = ncomp)
    fit <- y <- rep(namclas, m * ncomp)
    dummyfit <- NULL
    }
  ### END
  
  else {
    
    fm <- do.call(
      plsr, 
      c(list(Xr = Xr, Yr = dummy(Yr), Xu = Xu, 
        ncomp = ncomp, algo = algo, stor = stor), dots.algo)
      )
    
    m <- length(fm$fit$ncomp[fm$fit$ncomp == 1])
    dummyfit <- fm$fit[fm$fit$ncomp > 0, ] 
    
    # if ex-aequos, the first is selected
    fit <- dummyfit
    fit <- fit[, (ncol(fit) - nclas + 1):ncol(fit)]
    fit <- apply(fit, FUN = function(x) which.max(x), MARGIN = 1)
    fit <- sapply(fit, FUN = function(x) namclas[x])
    
    if (!is.null(Yu))
      y <- rep(as.character(Yu), ncomp)
    else
      y <- rep(NA, length(fit))
    
    fm <- fm$fm

    }
  
  r <- as.numeric(y != fit)

  dat <- data.frame(rownum = rep(1:m, ncomp), rownam = rep(rownam.Xu, ncomp),
    ncomp = sort(rep(1:ncomp, m)))
  
  y <- data.frame(dat, y, stringsAsFactors = FALSE)
  fit <- data.frame(dat, fit, stringsAsFactors = FALSE)
  r <- data.frame(dat, r)
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr
  
  if(!stor) fm <- NULL
  if(stor) {
    z <- sdod(Xr, Xu, fm) 
    fm$sd <- z$sdu
    fm$od <- z$odu
    }

  list(y = y, fit = fit, r = r, dummyfit = dummyfit, ni = ni, fm = fm)
    
  }
