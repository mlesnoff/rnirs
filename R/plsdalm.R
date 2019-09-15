plsdalm <- function(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, ...) {
  
  .pls <- match.fun(FUN = algo)

  dots <- list(...)
  namdot <- names(dots)
  
  z <- namdot[namdot %in% names(formals(.pls))]
  if(length(z) > 0) dots.pls <- dots[z] else dots.pls <- NULL
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  
  Xu <- .matrix(Xu)
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)
  
  colnam.Yr <- colnames(Yr)
  if(is.null(colnam.Yr)) colnam.Yr <- "y1"
  
  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)

  lev <- levels(Yr)
  namclas <- as.character(lev)

  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- rep(NA, m)
  
  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- y <- rep(namclas, m * ncomp)
    dummyfit <- NULL
    fm <- pca(Xr, Xu, ncomp = ncomp)
    }
  ### END
  
  else {
    
    Yrdummy <- dummy(Yr)
    
    param <- list(Xr = Xr, Yr = Yrdummy, Xu = Xu, ncomp = ncomp, algo = algo)
    param <- c(param, dots.pls)
    fm <- do.call(plsr, param)  
    
    z <- fm$fit
    dummyfit <- z[z$ncomp > 0, ] 
    
    # if ex-aequos, the first is selected
    z <- dummyfit
    z <- z[, (ncol(z) - nclas + 1):ncol(z)]
    z <- apply(z, FUN = function(x) which.max(x), MARGIN = 1)
    fit <- sapply(z, FUN = function(x) namclas[x])
    
    y <- rep(Yu, ncomp)
    
    }
  
  Tr <- fm$Tr
  Tu <- fm$Tu
  m <- nrow(Tu)
  
  P <- fm$P
  C <- fm$C
  R <- fm$R
  xmeans <- fm$xmeans
  ymeans <- fm$ymeans
  
  r <- as.numeric(y != fit)

  dat <- data.frame(rownum = rep(1:m, ncomp), rownam = rep(rownam.Xu, ncomp),
    ncomp = sort(rep(1:ncomp, m)))
  
  y <- data.frame(dat, y, stringsAsFactors = FALSE)
  fit <- data.frame(dat, fit, stringsAsFactors = FALSE)
  r <- data.frame(dat, r)
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr
  
  list(y = y, fit = fit, r = r, dummyfit = dummyfit, ni = ni,
    Tr = Tr, Tu = Tu, P = P, C = C, R = R, xmeans = xmeans, ymeans = ymeans,
    Xr = Xr, Xu = Xu)
    
  }
