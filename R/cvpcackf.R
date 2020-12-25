cvpcackf <- function(X, ncomp, algo = NULL, ...) {

    ### ckf-TRI algorithm: Saccenti & Camacho 2015 ("Algorithm3" p.469)
    ### = Simplification of the "Efficient ekf-TRI" (only column-wise removing)
    ### For ckf-TRI, an alternative to Alg3
    ### is Algorithm4 p.470 (Alg3 and Alg4 give equal results)
    ### but Alg4 can be time-consuming (see Table I)

    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    N <- n * p
    
    ncomp <- min(ncomp, n, p)
    zncomp <- seq(0, ncomp)
    
    if(is.null(algo))
        if(n < p)
            algo <- pca_eigenk
        else
            algo <- pca_eigen
    
    fm <- algo(X, ncomp = ncomp, ...)
    zX <- .center(X, fm$xmeans)
    ssr0 <- sum(zX * zX)

    E <- matrix(nrow = n, ncol = p)
    ssr <- numeric()
    for(a in seq_len(ncomp)) {
                
        R <- zX - tcrossprod(fm$T[, seq_len(a), drop = FALSE], 
                                                 fm$P[, seq_len(a), drop = FALSE])
        
        for(k in seq_len(p)) {
                    
            Qk <- crossprod(fm$P[k, seq_len(a)])
                    
            E[, k] <- zX[, k] * c(Qk) + R[, k]
                
            }
            
        ssr[a + 1] <- sum(E * E)
                
        }

     ssr[1] <- ssr0
        
     z <- data.frame(
         ncomp = zncomp,
         N = rep(N, ncomp + 1),
         ssr = ssr
         )
     z$msep <- z$ssr / N
     res <- z
     
     opt <- z$ncomp[z$msep == min(z$msep)]
     
     list(res = res, opt = opt)

     }


