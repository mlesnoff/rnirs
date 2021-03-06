.kpcdalm <- function(
    Xr, Yr, Xu, Yu = NULL, ncomp, 
    kern = kpol, 
    ...
    ) {

    Xr <- .matrix(Xr)
    zdim <- dim(Xr)
    n <- zdim[1]
    p <- zdim[2]
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)

    dots <- list(...)
    namdot <- names(dots)
    
    weights <- dots$weights
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- dots$weights
    
    z <- namdot[namdot %in% names(formals(kern))]
    if(length(z) > 0)
        dots.kern <- dots[z] 
    else 
        dots.kern <- NULL

    colnam.Y <- colnames(Yr)
    if(is.null(colnam.Y)) 
        colnam.Y <- "y1"
        
    Yr <- as.factor(Yr)
    ni <- c(table(Yr))
    nclas <- length(ni)

    # levels returns the sorted character level names 
    lev <- levels(Yr)            
    
    ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
    if(nclas == 1) {
        fm <- pca(Xr, Xu, ncomp = ncomp)
        y <- rep(as.character(Yu), ncomp)
        fit <- rep(lev, m * ncomp)
        dummyfit <- NULL
        }
    ### END
    
    else {
        
        fm <- do.call(
            .kpcr, 
            c(list(Xr = Xr, Yr = dummy(Yr), Xu = Xu, 
                ncomp = ncomp, kern = kern, weights = weights), dots.kern)
            )
        
        m <- length(fm$fit$ncomp[fm$fit$ncomp == 1])
        dummyfit <- fm$fit[fm$fit$ncomp > 0, ] 
        
        # if ex-aequos, the first is selected
        fit <- dummyfit
        fit <- fit[, seq(ncol(fit) - nclas + 1, ncol(fit))]
        fit <- apply(fit, FUN = .findmax, MARGIN = 1) 
        fit <- vapply(fit, FUN = function(x) lev[x], FUN.VALUE = "")
        
        if (!is.null(Yu))
            y <- rep(as.character(Yu), ncomp)
        else
            y <- rep(NA, length(fit))

        }
    
    r <- as.numeric(y != fit)

    dat <- data.frame(rownum = rep(seq_len(m), ncomp), rownam = rep(rownam.Xu, ncomp),
        ncomp = sort(rep(seq_len(ncomp), m)))
    
    y <- data.frame(dat, y, stringsAsFactors = FALSE)
    fit <- data.frame(dat, fit, stringsAsFactors = FALSE)
    r <- data.frame(dat, r)
    names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Y

    list(y = y, fit = fit, r = r,
        Tr = fm$Tr, Tu = fm$Tu, eig = fm$eig, sv = fm$sv,
        weights = fm$weights, cumpvar = fm$cumpvar, 
        ni = ni, dummyfit = dummyfit)             
        
    }