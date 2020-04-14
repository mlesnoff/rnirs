.detrend.als <- function(X, lambda = 1e+07, p = 0.001, maxit = 25) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  eps <- 1e-8
  fun <- function(x, lambda, p, eps, maxit) asysm(x, lambda, p, eps, maxit)
  zX <- t(apply(X, MARGIN = 1, FUN = fun, 
    lambda = lambda, p = p, eps = eps, maxit = maxit))

  dimnames(X) <- dimnam
  X
  
  }

.detrend.lowess <- function(X, f = 2/3, iter = 3) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  fun <- function(x, f, iter) lowess(x, f = f, iter = iter)$y
  zX <- t(apply(X, MARGIN = 1, FUN = fun, f = f, iter = iter))
  
  dimnames(X) <- dimnam
  X
  
  }

.detrend.poly <- function(X, degree = 1) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  y <- 1:ncol(X)
  fun <- function(x, y, degree) 
    resid(lm(x ~ stats::poly(y, degree = degree)))

  X <- t(apply(X, MARGIN = 1, FUN = fun, y = y, degree = degree))
  
  dimnames(X) <- dimnam
  X
  
  }

.dis <- function(mu, X, ...) {
  
  # Calculates the square of the Euclidean distances 
  # between the rows of X and vector mu
  
  X <- .matrix(X, ...)
  n <- nrow(X)
  p <- ncol(X)
  rownam <- row.names(X)

  X <- scale(X, center = mu, scale = FALSE)
  X <- X * X
  X <- matrix(rowSums(X), ncol = 1)

  row.names(X) <- rownam
  
  X
  
  }

.ellips <- function(shape, center = rep(0, ncol(shape)), radius = 1) {
  
  # The generated ellipse is: (x - mu)' * S^(-1) * (x - mu) <= r^2  
  # shape = variance-covariance matrix S (size q x q) of x (vector of length q)
  # center = mu (vector of length q)
  # radius = r
  
  theta <- seq(0, 2 * pi, length = 51)
  circ <- radius * cbind(cos(theta), sin(theta))
  
  z <- eigen(shape)
  d <- sqrt(z$values)
  V <- z$vectors
  
  X <- V %*% diag(d) %*% t(circ)
  
  X <- t(X + center)
  
  list(X = X, V = V, d = d)
  
  }

.findmax.ind <- function(x, seed = NULL) {
  ind <- which(x == max(x))
  n <- length(ind)
  if(n > 1) {
    set.seed(seed = seed)
    ind <- ind[sample(1:n, 1)]
    set.seed(seed = NULL)
    }
  ind
  }

.knnda <- function(Xr = NULL, Yr, Xu = NULL, Yu = NULL, weights = NULL) {

  colnam.Yr <- colnames(Yr)
  if(is.null(colnam.Yr)) colnam.Yr <- "y1"
  
  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)

  n <- length(Yr)
  
  # levels returns the sorted character level names 
  lev <- levels(Yr)      

  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- NA
  if(length(Yu) != 1) stop("Dimension of Yu must be 1x1.")
  
  if(is.null(weights)) weights <- rep(1, n)
  
  d <- weights
  d <- d / sum(d)
  
  dat <- data.frame(y = Yr, w = d)
  cnt <- dtaggregate(w ~ y, FUN = sum, data = dat)

  ind <- .findmax.ind(cnt$w)
  fit <- lev[ind]
  
  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- data.frame(rownum = 1, rownam = 1, y, stringsAsFactors = FALSE)
  fit <- data.frame(rownum = 1, rownam = 1, fit, stringsAsFactors = FALSE)
  r <- data.frame(rownum = 1, rownam = 1, r)
  names(cnt)[1] <- names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr
  
  list(y = y, fit = fit, r = r, ni = ni, cnt = cnt)  
  
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

.mah <- function(mu, X, U = NULL, ...) {
  
  # Square of the Mahalanobis distances between
  # the rows of X and the vector mu 
  
  X <- .matrix(X, ...)
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

.nipals <- function(X, weights = rep(1, nrow(X)), 
  tol = .Machine$double.eps^0.5, maxit = 100) {
  
  ## Find p such as ||X - t'p|| = min, with ||p|| = 1
  ## t = X %*% p
  
  t <- X[, which.max(.xvars(X, weights = weights))]
  ztol <- 1
  iter <- 1
    
  while(ztol > tol & iter <= maxit) {
    
    ## Regression of X on t
    p <- crossprod(weights * X, t) / sum(weights * t * t)
    p <- p / sqrt(sum(p * p))
        
    ## Regression of X' on p
    zt <- X %*% p
      
    ztol <- .xnorms(t - zt, weights = weights)
    t <- zt
    iter <- iter + 1
    
    }

  sv <- .xnorms(t, weights = weights)
  ## = norm of the score t
  ## = sqrt(sum(weight * t * t))
  
  list(t = c(t), p = c(p), sv = sv, niter = iter)
  
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

.wkern <- function(x, nam = "huber", a = 1.345) {
  
  switch(
    
    nam,
    
    bisquare = ifelse(abs(x) < a, (1 - (x / a)^2)^2, 0),
    
    cauchy = ifelse(abs(x) < a, 1 / (1 + (x / a)^2), 0),
    
    epan = ifelse(abs(x) < a, 1 - (x / a)^2, 0),
    
    gauss = ifelse(abs(x) < a, exp(-(x / a)^2), 0),
    
    huber =  ifelse(abs(x) < a, 1, a / abs(x)),
    
    invexp =  ifelse(abs(x) < a, 1 / exp(abs(x / a)), 0),
    
    talworth = ifelse(abs(x) < a, 1, 0),
    
    trian = ifelse(abs(x) < a, 1 - abs(x / a), 0),
    
    tricube = ifelse(abs(x) < a, (1 - abs(x / a)^3)^3, 0)

    )

  }

.xmeans <- function(X, weights = NULL, ...) {
  
  X <- .matrix(X, ...)
  
  if(is.null(weights))
    weights <- rep(1, nrow(X))
  
  colSums(weights * X) / sum(weights)   
  
  }

.xnorms <- function(X, weights = NULL, ...) {
  
  X <- .matrix(X, ...)
  
  if(is.null(weights))
    weights <- rep(1, nrow(X))
  
  sqrt(colSums(weights * X * X))
  
  }

.xvars <- function(X, weights = NULL, ...) {
  
  X <- .matrix(X, ...)
  
  if(is.null(weights))
    weights <- rep(1, nrow(X))
  
  xmeans <- .xmeans(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  colSums(weights * X * X)  / sum(weights)
  
  
  }

.xcor <- function(X, weights = NULL, ...) {

  X <- .matrix(X, ...)
  
  if(is.null(weights))
    weights <- rep(1, nrow(X))
  
  xmeans <- .xmeans(X, weights = weights)
  xvars <- .xvars(X, weights = weights)
  
  X <- scale(X, center = xmeans, scale = sqrt(xvars))
  
  crossprod(weights * X, X) / sum(weights)
  
  }

.xcov <- function(X, weights = NULL, ...) {
  
  X <- .matrix(X, ...)
  
  if(is.null(weights))
    weights <- rep(1, nrow(X))
  
  xmeans <- .xmeans(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  crossprod(sqrt(weights) * X) / sum(weights)
  
  }




