pls.nipals <- function(X, Y, ncomp) {
  
  X <- .matrix(X, prefix.colnam = "x")                                              
  n <- nrow(X)
  zp <- ncol(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")   
  zq <- ncol(Y)
  
  d <- rep(1 /n, n)

  xmeans <- colMeans(X)
  
  ymeans <- colMeans(Y)
  
  X <- scale(X, center = xmeans, scale = FALSE)                                     
  
  Y <- scale(Y, center = ymeans, scale = FALSE) 
  
  nam <- paste("comp", 1:ncomp, sep = "")
  T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))           
  R <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam)) 
  C <- matrix(nrow = zq, ncol = ncomp, dimnames = list(colnames(Y), nam))           
  TT <- vector(length = ncomp)

  res <- lapply(
    
    1:ncomp, function(a) {
  
      XY <- crossprod(X, Y)
      
      if(zq == 1) {
        w <- XY
        w <- w / sqrt(sum(w * w))
        }
      else {
        z <- svd(XY, nu = 1, nv = 0)
        w <- z$u
        }
      
      t <- X %*% w
      
      tt <- sum(t * t) 
      
      p <- crossprod(X, t) / tt
      
      c <- crossprod(Y, t)  / tt
      
      ## For saving time, we don't calculate matrix U
      ## Tenenhaus 1998 p.128 eq.2.2.5 (and same as in pls(oscorespls.fit))
      ## u <- Y %*% c / sum(c * c)
      ## Alterative (but give proportional vectors to u above)
      ## c <- z$v ; u <- Y %*% c
      ## end
      
      X <<- X - tcrossprod(t, p)
      
      Y <<- Y - tcrossprod(t, c) # the Y deflation is optional
      
      T[, a] <<- t
  
      W[, a] <<- w
  
      P[, a] <<- p
      C[, a] <<- c
      
      TT[a] <<- tt
      
      }
  
    )
    
  R <- W %*% solve(crossprod(P, W))

  list(T = T, P = P, W = W, C = C, R = R, TT = TT,
    xmeans = xmeans, ymeans = ymeans, weights = d)

  }
