odis <- function(fm, Xr, Xu = NULL, out = c("mad", "sd", "boxplot"), cri = 3) {
    
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  if(is.null(fm$Tr))
    fm$Tr <- fm$T
  
  out <- match.arg(out)
  
  X <- .matrix(Xr)
  n <- dim(X)[1]
  rownam <- row.names(X)
  
  X <- scale(X, center = fm$xmeans, scale = FALSE)
  
  ncomp <- dim(fm$Tr)[2]

  E <- X - tcrossprod(fm$Tr, fm$P)
  
  d <- sqrt(rowSums(E * E))
  cut <- switch(
    out, 
    mad = median(d) + cri * mad(d), 
    sd = mean(d) + cri * sd(d),
    boxplot = {z <- fivenum(d) ; z <- z[4] + 1.5 * diff(z[c(2, 4)])}
    )
  
  dr <- data.frame(rownum = 1:n, rownam = rownam, ncomp = rep(ncomp, n), d = d)
  dr$dstand <- dr$d / cut
  rownames(dr) <- 1:n
  
  ### NEW OBSERVATIONS
  
  Eu <- du <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam <- row.names(Xu)
    
    Tu <- .projscor(fm, Xu)
    Xu <- scale(Xu, center = fm$xmeans, scale = FALSE)
    
    Eu <- Xu - tcrossprod(Tu, fm$P)
    
    d <- sqrt(rowSums(Eu * Eu))
    
    du <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(ncomp, m), d = d)
    du$dstand <- du$d / cut
    rownames(du) <- 1:m
    
    }
  
  ### END

  list(dr = dr, du = du, cut = cut)

}