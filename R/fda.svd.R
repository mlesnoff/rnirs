fda.svd <- function(Xr, Yr, Xu = NULL, ncomp = NULL, ...) {
  
  X <- .matrix(Xr)
  n <- nrow(X)
  p <- ncol(X)
  
  Y <- as.factor(Yr)

  xmeans <- colMeans(X)
  X <- .center(X, xmeans)
  
  nclas <- length(unique(Y))
  
  if(is.null(ncomp)) 
    ncomp <- nclas - 1
  ncomp <- min(ncomp, p, nclas - 1)
  
  W <- matW(X, Y)$W
  z <- centr(X, Y)
  centers <- z$centers
  ni <- z$ni
  
  # Approach by row-weighted PCA (with weights ni/n) of the centers matrix
  # in metric W^(-1) (column-weigting, i.e. weighting of the variables, by tU)
  # ==> Cholesky decomposition of W^(-1) = U'U
  # and row-weighted (ni/n) SVD of Zcenters = centers * t(U)
  
  ## X = centered matrix
  ## W^(-1) = t(U) %*% U
  ## Zcenters = centers %*% t(U)
  ## Pz = Loadings of row-weighted (ni/n) PCA of Zcenters
  ## Tcenters = Zcenters %*% Pz =  centers %*% tU %*% Pz
  
  ## Coefficients of linear discriminants: 
  ## P = "LD" of lda(MASS) = Loadings for X in metric W(^-1)
  ## P = t(U) %*% Pz
  
  ## Z = X %*% t(U)
  ## T = Z %*% Pz = X %*% t(U) %*% Pz = X %*% P
  
  W <- W * n / (n - nclas)
  
  Winv <- solve(W)
  U <- chol(Winv)
  tU <- t(U) 

  Zcenters <- centers %*% tU

  zfm <- pca(Zcenters, ncomp = nclas - 1, weights = ni, ...)
  Pz <- zfm$P
  Tcenters <- Zcenters %*% Pz
  explvar <- zfm$explvar
  
  P <- tU %*% Pz[, 1:ncomp, drop = FALSE]
  
  T <- X %*% P

  Tu <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .center(.matrix(Xu), xmeans)
    m <- nrow(Xu)
    
    Tu <- Xu %*% P

    }  
  
  list(
    Tr = T, Tu = Tu, Tcenters = Tcenters,
    P = P, R = P,
    explvar = explvar, W = W, ni = ni
    )

}


