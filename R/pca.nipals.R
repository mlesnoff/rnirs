pca.nipals <- function(X, ncomp, gramschmidt = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 100) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)
  
  d <- rep(1 /n, n)

  xmeans <- colMeans(X, na.rm = TRUE)
  
  X <- scale(X, center = xmeans, scale = FALSE)
  
  x <- X
  cmeans <- xmeans
  nr <- n
  nc <- p
  xss <- rep(NA, length = ncomp)

  ########## CODE FROM FUNCTION "nipals" (PACKAGE "nipals"; K. Wright)
  
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
  eig <- sqrt(eig)
  #scores <- sweep(scores, 2, eig, "/")

  ########## END
  
  xss <- TotalSS * R2
  
  T <- scores
  R <- P <- loadings
  sv <- eig
  
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  colnames(T) <- colnames(P) <- nam
  

  list(T = T, P = P, R = R, sv = sv, xss = xss, xmeans = xmeans, weights = d, iter = iter)

  }




