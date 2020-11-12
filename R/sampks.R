sampks <- function(X, k, diss = c("euclidean", "mahalanobis", "correlation")) {

  diss <- match.arg(diss)
  
  X <- .matrix(X)
  n <- dim(X)[1]
  zn <- 1:n
  
  
  if(diss == "euclidean")
    D <- .dist(X)
  else
    D <- matdis(X, diss = diss)
  colnames(D) <- rownames(D) <- zn
  
  ## initial 2 selections (train)
  s <- which(D == max(D), arr.ind = TRUE)[1, ]
  ## candidates
  cand <- zn[-s]
  
  ## The following part is not time-efficient for k > 200
  for(i in 1:(k - 2)) {
    
    u <- matrixStats::colMins(D[s, cand, drop = TRUE])  
    
    zs <- cand[which(u == max(u))[1]]
    
    s <- c(s, zs)
    
    cand <- zn[-s]
    
    }
  ## End
  
  names(s) <- NULL
  
  list(train = s, test = cand)

  }
