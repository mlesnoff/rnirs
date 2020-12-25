xfit <- function(T, P, xmeans = rep(0, dim(P)[1])) {

    T <- .matrix(T)
    P <- .matrix(P)

    .center(tcrossprod(T, P), -xmeans)

    }
