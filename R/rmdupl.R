rmdupl <- function(X, nam = NULL, digits = NULL, check.all = FALSE) {
    
    X <- .matrix(X)
    X <- as.data.frame(X)

    z <- checkdupl(X = X, Y = X, nam = nam, digits = digits, check.all = FALSE)
    
    u <- z[z$rownum.X != z$rownum.Y, ]
    
    s <- NULL
    if(nrow(u)) {

        v <- t(u[, c("rownum.X", "rownum.Y")])
        for(i in seq_len(ncol(v))) 
            v[, i] <- sort(v[, i])
        v <- t(v)
        
        s <- sort(unique(v[, 2]))
        X <- X[-s, ]
        
        }
    
    list(X = X, rm = s)
    
}
