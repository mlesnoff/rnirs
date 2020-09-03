.center <- function(X, center = matrixStats::colMeans2(X)) 
  t((t(X) - c(center)))

.detrend.als <- function(X, lambda = 1e+07, p = 0.001, maxit = 25) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  eps <- 1e-8
  fun <- function(x, lambda, p, eps, maxit) asysm(x, lambda, p, eps, maxit)
  X <- X - t(apply(X, MARGIN = 1, FUN = fun, 
    lambda = lambda, p = p, eps = eps, maxit = maxit))

  dimnames(X) <- dimnam
  X
  
  }

.detrend.lowess <- function(X, f = 2/3, iter = 3) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  fun <- function(x, f, iter) lowess(x, f = f, iter = iter)$y
  X <- X - t(apply(X, MARGIN = 1, FUN = fun, f = f, iter = iter))
  
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

.dis <- function(X, mu, row = TRUE) {
  
  # Calculates the square of the Euclidean distances 
  # between the rows of X and vector mu
  
  X <- .matrix(X, row = row)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  rownam <- row.names(X)
  
  X <- .center(X, mu)
  X <- matrix(rowSums(X * X), ncol = 1)

  row.names(X) <- rownam
  
  X
  
  }

.dist <- function(X, Y = NULL) {
  
  ## Calculates squared Euclidean distances
  
  ## X = n x p
  ## Y = m x p
  ## ----- Y == NULL
  ## ==> n x n symetric matrix of the squared distances between the rows of X
  ## ----- Y != NULL
  ## ==> n x m matrix of the squared distances between the rows of X and the rows of Y
  
  X <- .matrix(X)
  
  if(is.null(Y)) {
    sq <- matrixStats::rowSums2(X * X)
    pmax(outer(sq, sq, "+") - 2 * tcrossprod(X), 0)
    }
  else {
    Y <- .matrix(Y)
    sqx <- matrixStats::rowSums2(X * X)
    sqy <- matrixStats::rowSums2(Y * Y)
    pmax(outer(sqx, sqy, "+") - 2 * tcrossprod(X, Y), 0)
    }
  
  }

.dw <- function(x) {
  x <- c(x)
  n <- length(x)
  sum((x[2:n] - x[1:(n - 1)])^2) / sum(x * x)
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

.eigpow <- function(X, tol = .Machine$double.eps^0.5, maxit = 30) {
  
    ztol <- 1
    iter <- 1
    
    v <- X[, which.max(.xvar(X))]
    
    while(ztol > tol & iter <= maxit) {
      
      zv <- X %*% v
      zv <- zv / sqrt(sum(zv * zv))

      ztol <- .xnorm(v - zv)
      v <- zv
      iter <- iter + 1

      }      
    
    list(v = c(v), niter = iter)
  
    }  

.findmax <- function(x, seed = NULL) {
  x <- which.max(x)
  n <- length(x)
  if(n > 1) {
    set.seed(seed = seed)
    x <- x[sample(1:n, 1)]
    set.seed(seed = NULL)
    }
  x
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

  ind <- .findmax(cnt$w)
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

.kurt <- function(x) {
  mean(((x - mean(x)) / sd(x))^4) - 3
  }

.mah <- function(X, mu, U = NULL, weights = NULL, row = TRUE) {
  
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
  
  .dis(zX, zmu)
  
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

  T <- .center(.matrix(X), fm$xmeans) %*% fm$R
    
  rownam <- row.names(X)
  colnam <- paste("comp", 1:dim(T)[2], sep = "")
  
  dimnames(T) <- list(rownam, colnam)
  
  T
  
  }

.resid.pls <- function(fm, Y, ncomp = NULL) {
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")
  
  zdim <- dim(fm$T) 
  n <-zdim[1]
  
  if(is.null(ncomp))
    ncomp <- zdim[2]

  ymeans <- fm$ymeans
  Ymeans <- matrix(rep(ymeans, n), nrow = n, byrow = TRUE)

  T <- fm$T[, 1:ncomp, drop = FALSE]
  B <- t(fm$C)[1:ncomp, , drop = FALSE]
  
  fit <- Ymeans + T %*% B
  
  r <- Y - fit 
  
  list(y = Y, fit = fit, r = r)
  
  }

.scale = function(X, center = rep(0, dim(X)[2]), scale) 
  t((t(X) - c(center)) / c(scale))

.simpp.hub <- function(X, nsim = 1000, seed = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  P <- tX <- t(X)
  if(nsim > 0) {
    
    zP <- matrix(nrow = p, ncol = nsim)
    
    set.seed(seed = seed)
    s1 <- sample(1:n, size = 50 * nsim, replace = TRUE)
    s2 <- sample(1:n, size = 50 * nsim, replace = TRUE)
    u <- which(s1 - s2 != 0)
    s1 <- s1[u][1:nsim]
    s2 <- s2[u][1:nsim]
    set.seed(seed = NULL)
    
    for(j in 1:nsim)
      zP[, j] <- tX[, s1[j]] - tX[, s2[j]]
      
    P <- cbind(P, zP)
    
    }
  
  P <- .scale(P, center = rep(0, dim(P)[2]), .xnorm(P)) 
  
  P

  }

.split <- function(x, n = 20, m = 10) {
  z <- c(1 / m * x, m * x)
  z1 <- seq(z[1], z[2], length.out = round(n / 2))
  z2 <- exp(seq(log(z[1]), log(z[2]), length.out = round(n / 2)))
  z2 <- z2[-c(1, length(z2))]
  z3 <- seq(.8, 1.2, by = .1) * x
  sort(unique(round(c(z1, z2, z3), digits = 10)))
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
  X <- .center(X, xmeans)
  
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
  
  X <- .scale(X, xmeans, sqrt(xvars))
  
  crossprod(sqrt(weights) * X)
  
  }

.xycor <- function(X, Y, weights = NULL, row = FALSE) {

  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  Y <- .matrix(Y, row = row)
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  xvars <- .xvar(X, weights = weights)
  
  ymeans <- .xmean(Y, weights = weights)
  yvars <- .xvar(Y, weights = weights)
  
  X <- .scale(X, xmeans, sqrt(xvars))
  Y <- .scale(Y, ymeans, sqrt(yvars))
  
  crossprod(weights * X, Y)
  
  }

.xcov <- function(X, weights = NULL, row = FALSE) {
  
  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)
  
  crossprod(sqrt(weights) * X)
  
  }

.xycov <- function(X, Y, weights = NULL, row = FALSE) {
  
  X <- .matrix(X, row = row)
  n <- dim(X)[1]
  
  Y <- .matrix(Y, row = row)
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  ymeans <- .xmean(Y, weights = weights)
  
  X <- .center(X, xmeans)
  Y <- .center(Y, ymeans)
  
  crossprod(weights * X, Y)
  
  }

.xmedspa <- function(X, delta = 1e-6) {
  
  X <- .matrix(X, row = FALSE)
  
  ##### COPY OF FUNCTION 'spatial.median' AVAILABLE IN THE SCRIPT PcaLocantore.R
  ##### OF PACKAGE rrcov v.1.4-3 on R CRAN (Thanks to V. Todorov, 2016)

  x <- X
  
  dime = dim(x)
  n=dime[1]
  p=dime[2]
  delta1=delta*sqrt(p)
  
  #mu0=apply(x,2,median)
  mu0=matrixStats::colMedians(x)
  
  h=delta1+1
  tt=0
  while(h>delta1){
    tt=tt+1
    TT=matrix(mu0,n,p,byrow=TRUE)
    U=(x-TT)^2
    
    #w=sqrt(apply(U,1,sum))
    w=sqrt(matrixStats::rowSums2(U))
    
    w0=median(w)
    ep=delta*w0

    z=(w<=ep)
    w[z]=ep
    w[!z]=1/w[!z]
    w=w/sum(w)
    x1=x
    for(i in 1:n)
      x1[i,]=w[i]*x[i,]
    
    #mu=apply(x1,2,sum)
    mu=matrixStats::colSums2(x1)
    
    h=sqrt(sum((mu-mu0)^2))
    mu0=mu
    }

  ##### END
  
  mu0
    
}



