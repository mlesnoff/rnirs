checkna <- function(X) {
  
  n <- nrow(X)
  rownam <- row.names(X)
  if(is.null(rownam)) rownam <- 1:n
  
  z <- rowSums(is.na(X))
  z <- data.frame(rownum = 1:n, rownam = rownam, nbna = z)
  z$all.na <- ifelse(z$nbna == ncol(X), TRUE, FALSE)
  z
  
}
  





