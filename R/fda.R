fda <- function(Xr, Yr, Xu = NULL, ncomp = NULL, pseudo = FALSE) {
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  p <- ncol(Xr)
  
  Yr <- as.factor(Yr)

  xmeans <- colMeans(Xr)
  Xr <- .center(Xr, xmeans)

  nclas <- length(unique(Yr))
  
  if(is.null(ncomp)) 
    ncomp <- nclas - 1
  
  ncomp <- min(ncomp, p, nclas - 1)
  
  z <- matB(Xr, Yr)
  B <- z$B
  centers <- z$centers
  ni <- z$ni
  
  W <- matW(Xr, Yr)$W
  W <- W * n / (n - nclas)
  
  if(!pseudo)
    Winv <- solve(W)
  else
    Winv <- pinv(W)$Xplus
  
  fm <- eigen(Winv %*% B)
  P <- fm$vectors[, 1:ncomp, drop = FALSE]
  eig <- fm$values[1:ncomp]
  P <- Re(P)
  eig <- Re(eig)
  
  norm.P <- sqrt(diag(t(P) %*% W %*% P))
  P <- .scale(P, scale = norm.P)
  colnames(P) <- paste("comp", 1:ncomp, sep = "")
  row.names(P) <- colnames(W)
  
  Tr <- Xr %*% P
  
  Tcenters <- centers %*% P
  
  explvar <- data.frame(ncomp = 1:ncomp, var = eig, pvar = eig / sum(eig))
  explvar$cumpvar <- cumsum(explvar$pvar)

  Tu <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .center(.matrix(Xu), xmeans)
    m <- nrow(Xu)
    
    Tu <- Xu %*% P

    }  
  
  list(
    Tr = Tr, Tu = Tu, Tcenters = Tcenters,
    P = P, R = P,
    explvar = explvar, W = W, ni = ni
    )

  }


