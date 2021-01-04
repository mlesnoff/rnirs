getknn <- function(
    Xr, Xu, k = NULL,
    diss = c("euclidean", "mahalanobis", "correlation"), 
    algorithm = "brute", list = TRUE
    ){
    
    diss <- match.arg(diss)
    
    Xr <- .matrix(Xr)
    n <- nrow(Xr)
    p <- ncol(Xr)
    
    Xu <- .matrix(Xu)
    m <- nrow(Xu)
    rownam.Xu <- row.names(Xu)

    if(is.null(k)) k <- n
    if(k > n) k <- n
    
    if(diss %in% c("euclidean", "mahalanobis")) {
        
        if(diss == "mahalanobis") {
        
            sigma <- cov(Xr)
            U <- chol(sigma)
            
            Uinv <- solve(U)
            
            Xr <- Xr %*% Uinv
            Xu <- Xu %*% Uinv
            
            }
        
        z <- FNN::get.knnx(Xr, Xu, k = k, algorithm = algorithm)

        nn <- z$nn.index
        d <- z$nn.dist
        
        }
    
    if(diss == "correlation") {

        z <- cor(t(Xr), t(Xu))
        D <- sqrt(.5 * (1 - z))         # z <- seq(-1, 1, .1) ; plot(z, sqrt(.5 * (1 - z)))
                                                                # D = sqrt(.5 * (1 - rho))
                                                                # rho = 1 - 2 * D^2
        
        z <- lapply(data.frame(D), function(x) order(x, decreasing = FALSE))
        znn <- data.frame(z)
        
        z <- lapply(data.frame(D), function(x) sort(x, decreasing = FALSE))
        zd <- data.frame(z)
        
        nn <- t(znn)[, seq_len(k), drop = FALSE]
        d <- t(zd)[, seq_len(k), drop = FALSE]

        }

    row.names(d) <- row.names(nn) <- rownam.Xu
    colnames(d) <- colnames(nn) <- seq_len(k)

    ##### MAKE lists
    
    if(list) {
        
        .fun <- function(x) {
            n <- nrow(x)
            z <- vector("list", length = n)
            for(i in seq_len(n)) z[[i]] <- x[i, seq_len(k)]
            names(z) <- row.names(x)
            z
            }
    
        listnn <- .fun(nn)
        listd <- .fun(d)
    
        }
    
    ##### END

    list(nn = nn, d = d, listnn = listnn, listd = listd)
    
    }