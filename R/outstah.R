outstah <- function(X, scale = TRUE, nsim = 1500) {
    
    X <- rnirs:::.matrix(X, row = FALSE)
    
    if(scale) {
        zmu <- matrixStats::colMedians(X)
        zs <- matrixStats::colMads(X)
        X <- rnirs:::.scale(X, zmu, zs)
        }
    
    #P <- .simpp.hub(X, nsim = nsim, seed = 1)
    P <- matrix(sample(0:1, ncol(X) * nsim, replace = TRUE), nrow = ncol(X))
    headm(P)

    T <- X %*% P
    
    mu <- matrixStats::colMedians(T)
    s <- matrixStats::colMads(T)
    T <- rnirs:::.scale(T, mu, s)
    
    r <- matrixStats::rowMaxs(abs(T))
    
    r    

    }