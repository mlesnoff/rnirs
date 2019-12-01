fda <- function(Xr, Yr, Xu = NULL, ncomp = NULL) {
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  p <- ncol(Xr)
  
  Yr <- as.factor(Yr)

  Xr <- scale(Xr, center = TRUE, scale = FALSE) # scale returns a matrix
  xmeans <- attr(Xr, "scaled:center")
  
  nclas <- length(unique(Yr))
  if(is.null(ncomp)) ncomp <- nclas - 1
  ncomp <- min(ncomp, p, nclas - 1)
  
  W <- matW(Xr, Yr)$W
  z <- matB(Xr, Yr)
  Xcenters <- z$centers
  Xc <- z$Xc
  
  # Compromise max p'Bp / p'Wp
  # i.e. to max p'Bp with constraint p'Wp = 1
  # ==> p are the eigenvectors of W^(-1)B
  # W^(-1)B * p = lambda * p   (lambda = mu in Saporta p.446)
  
  # Approach by PCA of the centers matrix (Xc) in metric W^(-1)
  # ==> Cholesky decomposition of W^(-1) = U'U
  # and SVD of Zc = Xc * t(U)
  
  # Use of the unbiased estimate of W
  W <- W * n / (n - nclas)
  
  Winv <- solve(W)
  U <- chol(Winv)
  tU <- t(U) 
  
  # Transformed data  
  Zr <- Xr %*% tU
  Zc <- Xc %*% tU
  Zcenters <- Xcenters %*% tU
  
  # PCA of Zc (==> loadings for Z)
  u <- pca(Zc, ncomp = nclas - 1)
  Pz <- u$P
  explvar <- u$explvar
  
  # Coefficients of linear discriminants: 
  # P = "LD" of lda(MASS) = Loadings for X
  P <- tU %*% Pz[, 1:ncomp, drop = FALSE]
  
  # Scores
  Tr <- Xr %*% P               # = Zr %*% Pz
  Tcenters <- Xcenters %*% P   # = Zcenters %*% Pz

  Zu <- Tu <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    Xu <- scale(Xu, center = xmeans, scale = FALSE)
    m <- nrow(Xu)
    
    Zu <- Xu %*% tU
    
    Tu <- Xu %*% P             # = Zu %*% Pz

    }  
  
  list(
    Tr = Tr, Tu = Tu, Tcenters = Tcenters,
    P = P, R = P, Pz = Pz,
    Zr = Zr, Zcenters = Zcenters, Zu = Zu,
    explvar = explvar, W = W
    )

}


