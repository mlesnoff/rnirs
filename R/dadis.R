dadis <- function(
    Xr, Yr, Xu, Yu = NULL,
    diss = c("euclidean", "mahalanobis", "correlation"), 
    sigma = NULL
    ){
    
    diss <- match.arg(diss)

    Xr <- .matrix(Xr)
    zdim <- dim(Xr)
    n <- zdim[1]
    p <- zdim[2]
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)
    
    colnam.Y <- colnames(Yr)
    if(is.null(colnam.Y)) 
        colnam.Y <- "y1"
    
    Yr <- as.factor(Yr)
    ni <- c(table(Yr))
    nclas <- length(ni)
    
    # levels returns the sorted character level names 
    lev <- levels(Yr)            
    
    if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- rep(NA, m)

    ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
    if(nclas == 1) {
        fit <- rep(lev, m)
        centers <- d <- NULL
        }
    ### END
    
    else {
    
        centers <- centr(Xr, Yr)$centers
        
        if(diss == "mahalanobis") {
            
            if(is.null(sigma))
                Wi <- matW(Xr, Yr)$Wi
            else{
                sigma <- as.matrix(sigma)
                Wi <- vector(length = nclas, mode = "list")
                for(i in seq_len(nclas))
                    Wi[[i]] <- sigma
                }
            }
                
        for(i in seq_len(nclas)) {
        
            if(diss %in% c("euclidean", "correlation"))
                zd <- dis(Xu, mu = centers[i, ], diss = diss)$dr$d
    
            if(diss == "mahalanobis") {
                
                ### IF SIGMA IS SINGULAR 
                ### ==> THE OPTION IS TO REPLACE SIGMA BY DIAG(SIGMA)
                U <- tryCatch(chol(Wi[[i]]), error = function(e) e)
                if(inherits(U, "error")) {
                    Wi[[i]] <- diag(diag(Wi[[i]]), nrow = p)
                    U <- sqrt(Wi[[i]])
                    }
                ### END
                
                zd <- .mah(Xu, centers[i, ], U)
                
                }
        
            if(i == 1) 
                d <- zd 
            else 
                d <- cbind(d, zd)
        
            }

    if(is.vector(d)) d <- matrix(d, nrow = 1)
    
    colnames(d) <- lev
    
    z <- apply(-d, FUN = .findmax, MARGIN = 1) 
    fit <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
    
    }
    
    y <- Yu
    r <- as.numeric(y != fit)
    
    y <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, y, stringsAsFactors = FALSE)
    fit <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, fit, stringsAsFactors = FALSE)
    r <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, r)
    names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Y
    
    list(y = y, fit = fit, r = r, d = d, centers = centers, ni = ni)
    
    }