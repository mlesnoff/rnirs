odis <- function(fm, Xr, Xu = NULL,  
  out = c("mad", "sd", "boxplot"), cri = 3) {
    
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  out <- match.arg(out)
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  rownam <- row.names(Xr)
  
  Xr <- scale(Xr, center = fm$xmeans, scale = FALSE)
  
  ncomp <- ncol(fm$Tr)

  Er <- Xr - tcrossprod(fm$Tr, fm$P)
  
  d <- sqrt(rowSums(Er * Er))
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
    m <- nrow(Xu)
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

  list(dr = dr, du = du, cut = cut) #, Er = Er, Eu = Eu

}