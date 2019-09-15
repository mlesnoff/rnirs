odis <- function(Xr, fm, Xu = NULL, ncomp = NULL, out = c("mad", "sd", "boxplot"), cri = 3) {
  
  if(is.null(fm$Tr))  fm$Tr <- fm$T
  
  out <- match.arg(out)
  
  X <- .matrix(Xr, prefix.colnam = "x")
  n <- nrow(X)
  rownam <- row.names(X)

  xmeans <- fm$xmeans
  T <- fm$Tr
  P <- fm$P
  
  X <- scale(X, center = xmeans, scale = FALSE)
  
  if(is.null(ncomp)) ncomp <- ncol(T)
  T <- T[, 1:ncomp, drop = FALSE]
  P <- P[, 1:ncomp, drop = FALSE]
  
  E <- X - tcrossprod(T, P)
  
  d <- sqrt(rowSums(E * E))
  cut <- switch(
    out, 
    mad = median(d) + cri * mad(d), 
    sd = mean(d) + cri * sd(d),
    boxplot = {u <- fivenum(d) ; u <- u[4] + 1.5 * diff(u[c(2, 4)])}
    )
  
  Er <- E
  z <- data.frame(rownum = 1:n, rownam = rownam, ncomp = rep(ncomp, n), d = d)
  z$dstand <- z$d / cut
  rownames(z) <- 1:n
  dr <- z
  
  ### NEW OBSERVATIONS
  
  Eu <- du <- NULL
  if(!is.null(Xu)) {
    
    X <- .matrix(Xu)
    m <- nrow(X)
    rownam <- row.names(X)
    
    T <- fm$Tu
    T <- T[, 1:ncomp, drop = FALSE]

    X <- scale(X, center = xmeans, scale = FALSE)
    E <- X - tcrossprod(T, P)
    
    d <- sqrt(rowSums(E * E))
    
    Eu <- E
    z <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(ncomp, m), d = d)
    z$dstand <- z$d / cut
    rownames(z) <- 1:m
    du <- z
    
    }
  
  ### END

  list(dr = dr, du = du, Er = Er, Eu = Eu)

}