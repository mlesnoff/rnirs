kpol <- function(X, Y = NULL, degree = 1, scale = 1, offset = 0) {
    
    if(is.null(Y))
        K <- (scale * tcrossprod(X) + offset)
    else
        K <- (scale * tcrossprod(X, Y) + offset)
    
    if(degree > 1) {
        zK <- K
        for(i in seq_len(degree - 1))
            K <- K * zK
        }
    
    K    
        
    }

krbf <- function(X, Y = NULL, sigma = 1) {
    exp(-.5 * .dist(X, Y) / sigma^2)
    }

ktanh <- function(X, Y = NULL, scale = 1, offset = 0)
    tanh(kpol(X, Y, degree = 1, scale, offset))











