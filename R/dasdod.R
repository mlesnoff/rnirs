dasdod <- function(Xr, Yr, Xu, Yu = NULL, ncomp, nmin = 5,    ...){
    
    if(nmin < 2) stop("\nArgument nmin must be >= 2.\n\n")
    
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
    
    if(!is.null(Yu)) 
        Yu <- as.character(Yu) else Yu <- rep(NA, m)
    
    if(length(ncomp) == 1) 
        ncomp <- rep(ncomp, nclas)

    ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
    if(nclas == 1) {
        fit <- rep(lev, m)
        pvarcla <- ncomp <- index <- zod <- zsd <- od <- sd <- NULL
        }
    ### END
    
    else {
            
        pvarcla <- cutod <- cutsd <- vector(length = nclas)
        od <- sd <- matrix(nrow = m, ncol = nclas)
        
        for(i in seq_len(nclas)) {
            
            s <- which(Yr == lev[i])
            ns <- length(s)
            
            if(ns < nmin) {
                
                od[, i] <- sd[, i] <- rep(NA, m)
                cutod[i] <- cutsd[i] <- NA
            
                }
            
            else {
            
                ncomp[i] <- min(ncomp[i], ns - 1, p - 1)
                
                fm <- pca(Xr[s, ], Xu, ncomp = ncomp[i], ...)
                
                z <- fm$explvar
                pvarcla[i] <- z$cumpvar[dim(z)[1]]
                
                z <- scordis(fm)
                sd[, i] <- z$du$d
                cutsd[i] <- z$cutoff
                
                z <- odis(fm, Xr[s, ], Xu)
                od[, i] <- z$du$d
                cutod[i] <- z$cutoff
            
                }

            }
        
        zsd <- .scale(sd, rep(0, nclas), cutsd)
        zod <- .scale(od, rep(0, nclas), cutod)
        
        rownames(zod) <- rownames(zsd) <- rownames(od) <- rownames(sd) <- rownam.Xu
        colnames(zod) <- colnames(zsd) <- colnames(od) <- colnames(sd) <- lev
        
        }
    
    theta <- seq(0, 1, by = .1)
    ntheta <- length(theta)
    index <- vector(length = ntheta, mode = "list")
    for(i in seq_len(ntheta))
        index[[i]] <- data.frame(sqrt(theta[i] * zsd^2 + (1 - theta[i]) * zod^2))
    index <- setDF(rbindlist(index))
    colnames(index) <- lev
    
    z <- apply(-index, FUN = .findmax, MARGIN = 1) 
    fit <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
    
    y <- Yu
    r <- as.numeric(y != fit)
    
    dat <- data.frame(
        rownum = rep(seq_len(m), ntheta),
        rownam = rep(rownam.Xu, ntheta),
        theta = sort(rep(theta, m))
        )    
    
    y <- cbind(dat, y, stringsAsFactors = FALSE)
    fit <- cbind(dat, fit, stringsAsFactors = FALSE)
    r <- cbind(dat, r)
    
    names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Y
    
    list(y = y, fit = fit, r = r, index = index, sd = sd, od = od,
        sdstand = zsd, odstand = zod, cutsd = cutsd, cutod = cutod, 
        ncomp = ncomp, pvarcla = pvarcla, ni = ni)
        
    }