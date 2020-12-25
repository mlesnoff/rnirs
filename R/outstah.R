outstah <- function(X, scale = TRUE, nsim = 1500) {
    
    X <- .matrix(X, row = FALSE)
    
    if(scale) {
        zmu <- matrixStats::colMedians(X)
        zs <- matrixStats::colMads(X)
        X <- .scale(X, zmu, zs)
        }
    
    P <- .simpp.hub(X, nsim = nsim, seed = 1)

    T <- X %*% P
    
    mu <- matrixStats::colMedians(T)
    s <- matrixStats::colMads(T)
    T <- .scale(T, mu, s)
    
    r <- matrixStats::rowMaxs(abs(T))
    
    r    

    }