odis <- function(fm, Xr, Xu = NULL, ncomp = NULL, 
                 typcut = c("param", "mad", "boxplot")) {
  
  if(is.null(fm$Tr))
    names(fm)[which(names(fm) == "T")] <- "Tr"
  
  typcut <- match.arg(typcut)
  
  X <- .matrix(Xr)
  n <- dim(X)[1]
  rownam <- row.names(X)
  
  if(is.null(ncomp))
    ncomp <- dim(fm$Tr)[2]
  else 
    ncomp <- min(ncomp, dim(fm$Tr)[2])
  
  X <- .center(X, fm$xmeans)

  E <- X - tcrossprod(fm$Tr[, 1:ncomp, drop = FALSE], fm$P[, 1:ncomp, drop = FALSE])
  
  d <- sqrt(rowSums(E * E))
  
  cutoff <- switch(
    typcut, 
    param = {
      z <- d^(2/3)
      (median(z) + mad(z) * qnorm(p = .975))^(3/2)
      },
    mad = median(d) + 2.5 * mad(d),
    boxplot = {
      z <- fivenum(d)
      z <- z[4] + 1.5 * diff(z[c(2, 4)])
      max(d[d <= z])
      }
    )  
  
  dstand <- d / cutoff 
  
  dr <- data.frame(rownum = 1:n, rownam = rownam, ncomp = rep(ncomp, n), 
    d = d, dstand = dstand)
  rownames(dr) <- 1:n
  
  ### NEW OBSERVATIONS
  
  Eu <- du <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam <- row.names(Xu)
    
    Tu <- .projscor(fm, Xu)
    Xu <- .center(Xu, fm$xmeans)
    
    E <- Xu - tcrossprod(Tu[, 1:ncomp, drop = FALSE], fm$P[, 1:ncomp, drop = FALSE])
    
    d <- sqrt(rowSums(E * E))
    
    dstand <- d / cutoff 
    
    du <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(ncomp, m), 
      d = d, dstand = dstand)
    rownames(du) <- 1:m
    
    }
  
  ### END

  list(dr = dr, du = du, cutoff = cutoff)

  }