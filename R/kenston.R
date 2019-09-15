kenston <- function(X, m, diss = c("euclidean", "mahalanobis", "correlation")) {

  diss <- match.arg(diss)
  
  X <- .matrix(X)
  n <- nrow(X)
  
  D <- matdis(X, diss = diss)
  colnames(D) <- rownames(D) <- 1:n

  s <- which(D == max(D), arr.ind = TRUE)[1, ]
  candidates <- (1:n)[-s]
  
  for(i in 1:(m - 2)) {
    
    zD <- D[s, candidates, drop = TRUE]
    # minimal dissimilarities of the candidates to the already selected obs.
    u <- apply(zD, MARGIN = 2, FUN = min) 
    # selection of the maximal value of these mininal dissimilarities
    zs <- as.numeric(names(u[u == max(u)]))[1] 
    s <- c(s, zs)
    candidates <- (1:n)[-s]
    
    }
  
  names(s) <- NULL
  s

  }
