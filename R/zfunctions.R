
.detrend <- function(X, degree) {
  
  dimnam <- dimnames(X)
  wav <- as.numeric(colnames(X))
  fresid <- function(x, wav, degree) resid(lm(x ~ stats::poly(wav, degree)))
  X <- apply(X, MARGIN = 1, FUN = fresid, wav = wav, degree = degree)
  X <- t(X)
  dimnames(X) <- dimnam
  X
  
  }

.dis <- function(X, mu) {
  
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

.mah <- function(X, mu, U = NULL) {
  
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
  
  .dis(zX, zmu)
  
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

.savgol <- function(X, m, p, n, ts) {
  
  dimnam <- dimnames(X)
  X <- apply(X, MARGIN = 1, 
    FUN = signal::sgolayfilt, n = n, p = p, m = m, ts = ts)
  X <- t(X)
  dimnames(X) <- dimnam
  X
  
  }

.snv <- function(X, center, scale) {
  
  X <- t(X)
  n <- ncol(X)
  if(center) xmeans <- colMeans(X) else xmeans <- rep(0, n)
  if(scale) xscales <- apply(X, MARGIN = 2, FUN = sd) else xscales <- rep(1, n)
  X <- scale(X, center = xmeans, scale = xscales)
  t(X)
  
  }

























