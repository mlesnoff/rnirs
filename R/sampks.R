sampks <- function(X, m, diss = c("euclidean", "mahalanobis", "correlation")) {

  diss <- match.arg(diss)
  
  X <- .matrix(X)
  n <- dim(X)[1]
  zn <- 1:n
  
  if(diss == "euclidean")
    D <- .dist(X)
  else
    D <- matdis(X, diss = diss)
  colnames(D) <- rownames(D) <- 1:n

  s <- which(D == max(D), arr.ind = TRUE)[1, ]
  candidates <- (1:n)[-s]
  
  for(i in 1:(m - 2)) {
    
    nam <- colnames(D[s, candidates, drop = TRUE])
    u <- matrixStats::colMins(D[s, candidates, drop = TRUE])  
    
    zs <- as.numeric(nam[which(u == max(u))[1]])
    
    s <- c(s, zs)
    
    candidates <- zn[-s]
    
    }
  
  names(s) <- NULL
  s

  }
