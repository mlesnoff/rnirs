fda <- function(Xr, Yr, Xu = NULL, ncomp = NULL, 
  pseudo = FALSE) {
  
  X <- .matrix(Xr)
  n <- nrow(X)
  p <- ncol(X)
  
  Y <- as.factor(Yr)

  X <- scale(X, center = TRUE, scale = FALSE)
  xmeans <- attr(X, "scaled:center")
  
  nclas <- length(unique(Y))
  
  if(is.null(ncomp)) 
    ncomp <- nclas - 1
  
  ncomp <- min(ncomp, p, nclas - 1)
  
  z <- matB(X, Y)
  B <- z$B
  centers <- z$centers
  ni <- z$ni
  
  W <- matW(X, Y)$W
  W <- W * n / (n - nclas)
  
  if(!pseudo)
    Winv <- solve(W)
  else
    Winv <- pinv(W)$Xplus
    #Winv <- pinv2(W, ncomp = 30)$Xplus
  
  fm <- eigen(Winv %*% B)
  P <- fm$vectors[, 1:(nclas - 1), drop = FALSE]
  eig <- fm$values[1:(nclas - 1)]
  P <- Re(P)
  eig <- Re(eig)
  
  norm.P <- sqrt(diag(t(P) %*% W %*% P))
  P <- scale(P, center = FALSE, scale = norm.P)
  attr(P,"scaled:scale") <- NULL
  ## Same as: P %*% diag(1 / norm.P)
  colnames(P) <- paste("comp", 1:ncomp, sep = "")
  row.names(P) <- colnames(W)
  
  T <- X %*% P
  
  Tcenters <- centers %*% P
  
  explvar <- data.frame(ncomp = 1:ncomp, var = eig, pvar = eig / sum(eig))
  explvar$cumpvar <- cumsum(explvar$pvar)

  Tu <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    Xu <- scale(Xu, center = xmeans, scale = FALSE)
    m <- nrow(Xu)
    
    Tu <- Xu %*% P

    }  
  
  list(
    Tr = T, Tu = Tu, Tcenters = Tcenters,
    P = P, R = P,
    explvar = explvar, W = W, ni = ni
    )

  }


