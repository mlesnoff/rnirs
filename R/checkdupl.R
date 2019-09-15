checkdupl <- function(X, Y, nam = NULL, digits = NULL, check.all = FALSE) {
  
  X <- .matrix(X)
  Y <- .matrix(Y)
  
  n <- nrow(X)
  m <- nrow(Y)
  rownam.X <- row.names(X)
  rownam.Y <- row.names(Y)

  X <- as.data.frame(X)
  Y <- as.data.frame(Y)
  
  if(!is.null(nam)) nam <- as.character(nam) else nam <- names(X)
  
  u <- X[, nam]
  if(!is.null(digits)) u <- round(u, digits = digits)
  u$rownum.X <- rep(1:n)
  u$rownam.X <- rownam.X
  zref <- u
  
  u <- Y[, nam]
  if(!is.null(digits)) u <- round(u, digits = digits)
  u$rownum.Y <- rep(1:m)
  u$rownam.Y <- rownam.Y
  z <- u
  
  u <- merge(zref, z, by = nam, all = FALSE, allow.cartesian = TRUE)
  nam <- c(nam, "rownum.X", "rownum.Y", "rownam.X", "rownam.Y")
  u <- u[, nam]
  if(check.all)
    u$all.equal <- (rowSums(abs(X[u$rownum.X, ])) == rowSums(abs(Y[u$rownum.Y, ])))

  u <- u[order(u$rownum.X), ]
  
  u
  
}
