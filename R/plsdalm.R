plsdalm <- function(
    Xr, Yr, Xu, Yu = NULL, ncomp, 
    algo = NULL, 
    ...
    ) {

    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)

    if(is.null(algo))
        algo <- pls_kernel

    dots <- list(...)
    namdot <- names(dots)
    
    z <- namdot[namdot %in% names(formals(algo))]
    if(length(z) > 0) dots.algo <- dots[z] else dots.algo <- NULL

    colnam.Y <- colnames(Yr)
    if(is.null(colnam.Y)) colnam.Y <- "y1"
        
    Yr <- as.factor(Yr)
    nclas <- length(unique(Yr))
    
    ni <- c(table(Yr))
    nclas <- length(ni)

    # levels returns the sorted character level names 
    lev <- levels(Yr)            
    
    ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
    if(nclas == 1) {
        fm <- pca(Xr, Xu, ncomp = ncomp)
        y <- rep(as.character(Yu), ncomp)
        fit <- rep(lev, m * ncomp)
        zdumfit <- NULL
        }
    ### END
    
    else {
        
        fm <- do.call(
            plsr, 
            c(list(Xr = Xr, Yr = dummy(Yr), Xu = Xu, 
                ncomp = ncomp, algo = algo), dots.algo)
            )
        
        m <- length(fm$fit$ncomp[fm$fit$ncomp == 1])
        zdumfit <- fm$fit[fm$fit$ncomp > 0, ] 
        
        # if ex-aequos, the first is selected
        k <- ncol(zdumfit)
        fit <- zdumfit[, seq(k - nclas + 1, k)]
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
    
    dummyfit <- list()
    for(i in seq_len(ncomp)) {
        u <- which(zdumfit$ncomp == i)
        dummyfit[[i]] <- zdumfit[u, -(seq_len(ncol(dat)))]
        row.names(dummyfit[[i]]) <- rownam.Xu
        }

    list(y = y, fit = fit, r = r,
        Tr = fm$Tr, Tu = fm$Tu, P = fm$P, W = fm$W, R = fm$R, C = fm$C, TT = fm$TT,
        xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights,
        T.ortho = fm$T.ortho, dummyfit = dummyfit, ni = ni)             
        
    }