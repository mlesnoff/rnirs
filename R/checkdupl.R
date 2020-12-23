checkdupl <- function(X, Y, nam = NULL, digits = NULL, check.all = FALSE) {
  
    X <- .matrix(X)
    Y <- .matrix(Y)
  
    n <- nrow(X)
    m <- nrow(Y)
    rownam.X <- row.names(X)
    rownam.Y <- row.names(Y)

    X <- as.data.frame(X)
    Y <- as.data.frame(Y)
  
    if(!is.null(nam)) 
        nam <- as.character(nam)
    else 
        nam <- names(X)
  
    u <- X[, nam]
    if(!is.null(digits)) 
        u <- round(u, digits = digits)
    u$rownum.X <- rep(seq_len(n))
    u$rownam.X <- rownam.X
    zref <- u
  
    u <- Y[, nam]
    if(!is.null(digits)) 
        u <- round(u, digits = digits)
    u$rownum.Y <- rep(seq_len(m))
    u$rownam.Y <- rownam.Y
    z <- u
  
    z <- merge(zref, z, by = nam, all = FALSE, allow.cartesian = TRUE)
    nam <- c(nam, "rownum.X", "rownum.Y", "rownam.X", "rownam.Y")
    z <- z[, nam]
    if(check.all)
        z$all.equal <- (rowSums(abs(X[z$rownum.X, ])) == rowSums(abs(Y[z$rownum.Y, ])))

    z <- z[order(z$rownum.X), ]
  
    z
  
    }
