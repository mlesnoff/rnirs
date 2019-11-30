.detrend.als <- function(X, lambda = 1e+07, p = 0.001, maxit = 25, baseline = FALSE) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  eps <- 1e-8
  fun <- function(x, lambda, p, eps, maxit) asysm(x, lambda, p, eps, maxit)
  zX <- t(apply(X, MARGIN = 1, FUN = fun, 
    lambda = lambda, p = p, eps = eps, maxit = maxit))

  if(baseline) X <- zX else X <- X - zX
  
  dimnames(X) <- dimnam
  X
  
  }

.detrend.lowess <- function(X, f = 2/3, iter = 3, baseline = FALSE) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  fun <- function(x, f, iter) lowess(x, f = f, iter = iter)$y
  zX <- t(apply(X, MARGIN = 1, FUN = fun, f = f, iter = iter))

  if(baseline) X <- zX else X <- X - zX
  
  dimnames(X) <- dimnam
  X
  
  }

.detrend.poly <- function(X, degree = 1, baseline = FALSE) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  if(baseline) 
    zf <- fitted 
  else 
    zf <- resid
  
  y <- 1:ncol(X)
  fun <- function(x, y, degree) zf(lm(x ~ stats::poly(y, degree = degree)))

  X <- t(apply(X, MARGIN = 1, FUN = fun, y = y, degree = degree))
  
  dimnames(X) <- dimnam
  X
  
  }

.dis <- function(mu, X) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)
  rownam <- row.names(X)
  
  X <- scale(X, center = mu, scale = FALSE)
  X <- X * X
  X <- matrix(rowSums(X), ncol = 1)

  row.names(X) <- rownam
  
  X
  
  }

.knnda <- function(Xr = NULL, Yr, Xu = NULL, Yu = NULL, weights = NULL) {

  colnam.Yr <- colnames(Yr)
  if(is.null(colnam.Yr)) colnam.Yr <- "y1"

  Yr <- as.factor(Yr)
  namclas <- as.character(levels(Yr))

  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- NA
  if(length(Yu) != 1) stop("Dimension of Yu must be 1x1.")
  
  if(is.null(weights)) weights <- rep(1, n)
  
  d <- weights
  d <- d / sum(d)
  
  dat <- data.frame(y = Yr, w = d)
  cnt <- dtaggregate(w ~ y, FUN = sum, data = dat)

  ind <- which(cnt$w == max(cnt$w))
  n <- length(ind)
  set.seed(seed = 1)
  if(n > 1) ind <- sample(1:n, 1)
  set.seed(seed = NULL)
  fit <- namclas[ind]
  
  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- data.frame(rownum = 1, rownam = 1, y, stringsAsFactors = FALSE)
  fit <- data.frame(rownum = 1, rownam = 1, fit, stringsAsFactors = FALSE)
  r <- data.frame(rownum = 1, rownam = 1, r)
  names(cnt)[1] <- names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr
  
  list(y = y, fit = fit, r = r, cnt = cnt)  
  
  }

.knnr <- function(Xr = NULL, Yr, Xu = NULL, Yu = NULL, weights = NULL) {
  
  if(!is.matrix(Yr)) stop("Yr must be a matrix.")
  n <- nrow(Yr)
  q <- ncol(Yr)
  if(is.null(colnames(Yr))) colnames(Yr) <- paste("y", 1:ncol(Yr), sep = "")
  colnam.Yr <- colnames(Yr)

  if(is.null(Yu)) Yu <- matrix(nrow = 1, ncol = q)
    else Yu <- matrix(Yu, nrow = 1, ncol = q)
  colnames(Yu) <- colnam.Yr
  
  if(is.null(weights)) weights <- rep(1, n)
  
  d <- weights
  d <- d / sum(d)
  
  ymeans <- colSums(Yr * d)    # (Y * d = D %*% Y)
  
  y <- Yu
  fit <- matrix(ymeans, nrow = 1) ; colnames(fit) <- colnam.Yr
  r <- y - ymeans
  
  y <- data.frame(rownum = 1, rownam = 1, y)
  fit <- data.frame(rownum = 1, rownam = 1, fit)
  r <- data.frame(rownum = 1, rownam = 1, r)
  
  zq <- ncol(y)
  u <- (zq - q + 1):zq
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Yr
  
  list(y = y, fit = fit, r = r)  

  }

.mah <- function(mu, X, U = NULL) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)
  rownam <- row.names(X)
  
  if(is.null(U))
    U <- chol(cov(X))
  else 
    U <- as.matrix(U)
  
  Uinv <- solve(U)
  
  zX <- X %*% Uinv
  zmu <- mu %*% Uinv
  
  .dis(zmu, zX)
  
  }

.matrix <- function(X, row = TRUE,  prefix.colnam = "x") {
  
  if(is.vector(X)) 
    if(row) X <- matrix(X, nrow = 1)
      else X <- matrix(X, ncol = 1)
  
  if(!is.matrix(X)) X <- as.matrix(X)
  
  if(is.null(row.names(X))) row.names(X) <- 1:nrow(X)
  
  if(is.null(colnames(X))) colnames(X) <- paste(prefix.colnam, 1:ncol(X), sep = "")
  
  X
  
  }

.varw <- function(x, weights = rep(1, length(x))) {
  
  d <- weights / sum(weights)
  
  xmean <- sum(d * x)
  
  sum(d * (x - xmean)^2)
  
  }

.projscor <- function(fm, X) {
  
  ## fm = Output of functions pca or pls, 
  ## or of the PCA or PLS algorithm functions

  T <- scale(.matrix(X), center = fm$xmeans, scale = FALSE) %*% fm$R
    
  rownam <- row.names(X)
  colnam <- paste("comp", 1:ncol(T), sep = "")
  
  dimnames(T) <- list(rownam, colnam)
  
  T
  
  }
