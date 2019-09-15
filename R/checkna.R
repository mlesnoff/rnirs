checkna <- function(X) {
  
  n <- nrow(X)
  rownam <- row.names(X)
  if(is.null(rownam)) rownam <- 1:n
  
  z <- is.na(X)
  u <- rowSums(z)
  u <- data.frame(rownum = 1:n, rownam = rownam, nbna = u)
  u$all.na <- ifelse(u$nbna == ncol(X), TRUE, FALSE)
  u
  
}
  





