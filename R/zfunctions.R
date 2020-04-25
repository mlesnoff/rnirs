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
  
  y <- 1:dim(X)[2]
  fun <- function(x, y, degree) 
    resid(lm(x ~ stats::poly(y, degree = degree)))

  X <- t(apply(X, MARGIN = 1, FUN = fun, y = y, degree = degree))
  
  dimnames(X) <- dimnam
  X
  
  }

.dis <- function(mu, X, row = TRUE) {
  
  # Calculates the square of the Euclidean distances 
  # between the rows of X and vector mu
  
  X <- .matrix(X, row = row)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
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

.fweights <- function(x, nam = "huber", a = 1.345) {
  
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
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  dat <- data.frame(y = Yr, w = weights)
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
  n <- dim(Yr)[1]
  q <- dim(Yr)[2]
  
  if(is.null(colnames(Yr))) colnames(Yr) <- paste("y", 1:dim(Yr)[2], sep = "")
  colnam.Yr <- colnames(Yr)

  if(is.null(Yu)) Yu <- matrix(nrow = 1, ncol = q)
    else Yu <- matrix(Yu, nrow = 1, ncol = q)
  colnames(Yu) <- colnam.Yr
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  ymeans <- colSums(Yr * weights)    # (Y * d = D %*% Y)
  
  y <- Yu
  fit <- matrix(ymeans, nrow = 1) ; colnames(fit) <- colnam.Yr
  r <- y - ymeans
  
  y <- data.frame(rownum = 1, rownam = 1, y)
  fit <- data.frame(rownum = 1, rownam = 1, fit)
  r <- data.frame(rownum = 1, rownam = 1, r)
  
  zq <- dim(y)[2]
  u <- (zq - q + 1):zq
  names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Yr
  
  list(y = y, fit = fit, r = r)  

  }

.mah <- function(mu, X, U = NULL, weights = NULL, row = TRUE) {
  
  # Calculate the square of the Mahalanobis distances between
  # the rows of X and the vector mu 
  
  X <- .matrix(X, row = row)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  rownam <- row.names(X)

  if(is.null(U)) {
    if(is.null(weights))
      S <- cov(X) * (n - 1) / n
    else 
      S <- .xcov(X, weights = weights)
    U <- chol(S)
    }
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
  
  if(is.null(row.names(X))) row.names(X) <- 1:dim(X)[1]
  
  if(is.null(colnames(X))) colnames(X) <- paste(prefix.colnam, 1:dim(X)[2], sep = "")
  
  X
  
  }

.nipals <- function(X, weights = NULL, 
  tol = .Machine$double.eps^0.5, maxit = 100) {
  
  ## Find p such as ||X - t'p|| = min, with ||p|| = 1
  ## t = X %*% p
  
  n <- dim(X)[1]

  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  t <- X[, which.max(.xvar(X, weights = weights))]
  ztol <- 1
  iter <- 1
    
  while(ztol > tol & iter <= maxit) {
    
    ## Regression of X on t
    p <- crossprod(weights * X, t) / sum(weights * t * t)
    p <- p / sqrt(sum(p * p))
        
    ## Regression of X' on p
    zt <- X %*% p
      
    ztol <- .xnorm(t - zt)
    t <- zt
    iter <- iter + 1
    
    }

  sv <- .xnorm(t, weights = weights)
  
  list(t = c(t), p = c(p), sv = sv, niter = iter)
  
  }

.projscor <- function(fm, X) {
  
  ## fm = Output of functions pca or pls, 
  ## or of the PCA or PLS algorithm functions

  T <- scale(.matrix(X), center = fm$xmeans, scale = FALSE) %*% fm$R
    
  rownam <- row.names(X)
  colnam <- paste("comp", 1:dim(T)[2], sep = "")
  
  dimnames(T) <- list(rownam, colnam)
  
  T
  
  }

.simpp.hub <- function(X, nrep) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  tX <- t(X)
  P <- tX
  if(nrep > 0) {
  
    for(j in 1:nrep) {
      
      K <- 2 * n
      s1 <- sample(1:n, size = K, replace = TRUE)
      s2 <- sample(1:n, size = K, replace = TRUE)
      u <- which(s1 - s2 != 0)
      s1 <- s1[u][1:n]
      s2 <- s2[u][1:n]
      
      zP <- tX[, s1] - tX[, s2]
      
      P <- cbind(P, zP)
      
      }
    
    }
  
  P <- scale(P, center = FALSE, scale = .xnorm(P)) 
  attributes(P)["scaled:scale"] <- NULL
  
  P

  }

.stahel <- function(X, P, scal = c("mad", "sd")) {

  scal <- switch(
    match.arg(scal),
    mad = mad,
    sd = sd
    )
  
  T <- X %*% P
  
  mu <- apply(T, 2, median)
  s <- apply(T, 2, scal)
  
  T <- scale(T, center = mu, scale = s)
  
  r <- apply(abs(T), 1, max)
  
  r
  
  }

.xmean <- function(X, weights = NULL, row = FALSE) {
  
  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  colSums(weights * X)   
  
  }

.xnorm <- function(X, weights = NULL, row = FALSE) {
  
  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  if(is.null(weights)) 
    weights <- rep(1, n)
  
  sqrt(colSums(weights * X * X))
  
  }

.xvar <- function(X, weights = NULL, row = FALSE) {
  
  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  colSums(weights * X * X)  / sum(weights)
  
  
  }

.xcor <- function(X, weights = NULL, row = FALSE) {

  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  xvars <- .xvar(X, weights = weights)
  
  X <- scale(X, center = xmeans, scale = sqrt(xvars))
  
  crossprod(weights * X, X)
  
  }

.xcov <- function(X, weights = NULL, row = FALSE) {
  
  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  crossprod(sqrt(weights) * X)
  
  }

.xmedspa <- function(X, delta = 1e-6) {
  
  X <- .matrix(X, row = FALSE)
  
  ##### COPY OF FUNCTION 'spatial.median' AVAILABLE IN THE SCRIPT PcaLocavntore.R
  ##### OF PACKAGE rrcov v.1.4-3 on R CRAN (V. Todorov, 2016)

  x <- X
  
  dime = dim(x)
  n=dime[1]
  p=dime[2]
  delta1=delta*sqrt(p)
  mu0=apply(x,2,median)
  h=delta1+1
  tt=0
  while(h>delta1){
    tt=tt+1
    TT=matrix(mu0,n,p,byrow=TRUE)
    U=(x-TT)^2
    w=sqrt(apply(U,1,sum))
    w0=median(w)
    ep=delta*w0

    z=(w<=ep)
    w[z]=ep
    w[!z]=1/w[!z]
    w=w/sum(w)
    x1=x
    for(i in 1:n)
      x1[i,]=w[i]*x[i,]
    mu=apply(x1,2,sum)
    h=sqrt(sum((mu-mu0)^2))
    mu0=mu
    }

  ##### END
  
  mu0
    
}



