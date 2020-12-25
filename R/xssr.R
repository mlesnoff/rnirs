xssr <- function(X, T, P, xmeans = rep(0, dim(P)[1])) {
    
    ### Unweighted SSR for Xfit
    ### (Including the component 'ncomp = 0')
    ### The output vector 'ssr' has length ncomp + 1

    X <- .matrix(X)
    T <- .matrix(T)
    P <- .matrix(P)
    ncomp <- ncol(T)
    
    X0 <- .center(X, xmeans)
    ssr <- sum(X0 * X0)
    for(a in seq_len(ncomp)) {
        E <- X - xfit(T[, seq_len(a), drop = FALSE], P[, seq_len(a), drop = FALSE], xmeans)
        ssr[a + 1] <- sum(E * E)
        }

    ssr
    
    }
