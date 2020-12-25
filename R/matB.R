matB <- function(X, y) {
    
    X <- .matrix(X)
    n <- nrow(X)
    p <- ncol(X)

    y <- as.factor(y)
    ni <- tabulate(y)
    nclas <- length(ni)
    
    lev <- levels(y)
    namy <- as.character(lev)
    
    zy <- as.numeric(y)
    
    centers <- centr(X, y)$centers

    B <- .xcov(centers, weights = ni)

    colnames(B) <- rownames(B) <- colnames(centers)

    list(B = B, ni = ni, centers = centers)
    
    }
