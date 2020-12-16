pca_nipalsna <- function(X, ncomp, 
  gs = TRUE, tol = .Machine$double.eps^0.5, maxit = 200) {
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  ncomp <- min(ncomp, n, p)
  
  xmeans <- colMeans(X, na.rm = TRUE)
  X <- .center(X, xmeans)
  
  x <- X
  cmeans <- xmeans
  nr <- n
  nc <- p
  eig <- rep(NA, length = ncomp)
  
  gramschmidt <- gs

  ##################### FUNCTION "nipals" OF PACKAGE "nipals" v0.7 (K. Wright, CRAN)
  ##################### = NIPALS WITH NA VALUES
  
  ##################### START 
  
  x.orig <- x
  col.na.count <- apply(x, 2, function(x) sum(!is.na(x)))
  if (any(col.na.count == 0)) 
    stop("At least one column is all NAs")
  row.na.count <- apply(x, 1, function(x) sum(!is.na(x)))
  if (any(row.na.count == 0)) 
    stop("At least one row is all NAs")
  
  TotalSS <- sum(x * x, na.rm = TRUE)
  PPp = matrix(0, nrow = nc, ncol = nc)
  TTp = matrix(0, nrow = nr, ncol = nr)
  eig <- rep(NA, length = ncomp)
  R2cum <- rep(NA, length = ncomp)
  loadings <- matrix(nrow = nc, ncol = ncomp)
  scores <- matrix(nrow = nr, ncol = ncomp)
  iter <- rep(NA, length = ncomp)
  
  x.miss <- is.na(x)
  has.na <- any(x.miss)
  
  for (h in 1:ncomp) {
    
    scol <- which.max(apply(x, 2, var, na.rm = TRUE))
    
    if (has.na) {
      x0 <- x
      x0[x.miss] <- 0
      th <- x0[, scol]
      }
    else 
      th <- x[, scol]
    
    pciter <- 1
    continue <- TRUE
    while (continue) {
      
      if (has.na) {
        T2 <- matrix(th * th, nrow = nr, ncol = nc)
        T2[x.miss] <- 0
        ph <- crossprod(x0, th)/colSums(T2)
        }
      else ph = crossprod(x, th)/sum(th * th)
      
      if (gramschmidt && h > 1) 
        ph <- ph - PPp %*% ph
      
      ph <- ph/sqrt(sum(ph * ph, na.rm = TRUE))
      th.old <- th
      
      if (has.na) {
        P2 <- matrix(ph * ph, nrow = nc, ncol = nr)
        P2[t(x.miss)] <- 0
        th = x0 %*% ph / colSums(P2)
        }
      else th = x %*% ph / sum(ph * ph)
            
      if (gramschmidt && h > 1)
        th <- th - TTp %*% th
            
      if (sum(abs(th - th.old), na.rm = TRUE) < tol) continue <- FALSE
      #if (sum((th - th.old)^2, na.rm = TRUE) < tol) continue <- FALSE
      
      pciter <- pciter + 1
      if (pciter == maxit) continue <- FALSE

      }

    x <- x - (th %*% t(ph))
    loadings[, h] <- ph
    scores[, h] <- th
    eig[h] <- sum(th * th, na.rm = TRUE)
    iter[h] <- pciter
    if (gramschmidt) {
      PPp = PPp + tcrossprod(ph)
      TTp = TTp + tcrossprod(th) / eig[h]
      }
    R2cum[h] <- 1 - (sum(x * x, na.rm = TRUE) / TotalSS)
    
    }
    
  R2 <- c(R2cum[1], diff(R2cum))
  
  ##################### END
  
  sv <- sqrt(eig) / sqrt(n)

  eig <- sv^2
  
  row.names(scores) <- row.names(X)
  row.names(loadings) <- colnames(X)
  
  colnames(scores) <- colnames(loadings) <- paste("comp", 1:ncomp, sep = "")

  list(T = scores, P = loadings, R = loadings,
    sv = sv, eig = eig, xmeans = xmeans, weights = rep(1, n), iter = iter, T.ortho = TRUE)

  }




