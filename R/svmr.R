svmr <- function(
    Xr, Yr, Xu, Yu = NULL,
    C = 1, epsilon = .1, 
    kern = krbf, 
    print = TRUE, 
    ...
    ) {
    
    if(is.character(kern)) {
        namkern <- kern
        kern <- get(kern)
        }
    else
        namkern <- as.character(substitute(kern))
    
    Xr <- .matrix(Xr)
    Xu <- .matrix(Xu)
    
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)
    
    q <- 1
    colnam.Yu <- "y1"
    
    C <- sort(unique(C))
    epsilon <- sort(unique(epsilon))
    
    dots <- list(...)
    
    z <- formals(kern)
    nam <- names(z)
    nam <- nam[-match(c("X", "Y"), nam)]
    z <- z[nam]
    ndots <- length(dots)
    if(ndots > 0)
        for(i in seq_len(ndots))
            if(names(dots[i]) %in% nam)
                z[[names(dots[i])]] <- dots[[i]]
    listkpar <- lapply(z, FUN = function(x) sort(unique(x)))
    
    listpar <- c(list(C = C, epsilon = epsilon), listkpar) 
    param <- expand.grid(listpar)
    npar <- ncol(param)
    
    zkern <- switch(
        namkern,
        krbf = rbfdot,
        kpol = polydot,
        ktanh = tanhdot
        )
    
    r <- fit <- y <- vector(mode = "list", length = npar)
    
    if(print){
        cat(paste("\n Kernel: ", namkern, "\n", sep = ""))
        cat("\n Parameters: \n")
        print(listpar)
        cat(paste("\n Nb combinations: ", nrow(param), "\n\n", sep = ""))
        }
    
    r <- fit <- y <- vector(mode = "list", length = npar)

    for(i in seq_len(nrow(param))) {
        
        z <- param[i, ]
        
        if(print)
            print(z)
            #cat(i, "")
        
        if("sigma" %in% names(z))
            z$sigma <- 1 / (2 * z$sigma^2)
        ### See ?kernlab::rbfdot
        ### sigma <- 17 ; krbf(seq_len(10), seq(2, 11), sigma = sigma) ; rbfdot(1 / (2 * sigma^2))(seq_len(10), seq(2, 11))
        ### sigma.rbfdot = 1 / (2 * sigma.krbf^2)
        
        zkpar <- z[, -match(c("C", "epsilon"), names(z)), drop = FALSE]
        fkern <- do.call(zkern, zkpar)

        res <- kernlab::ksvm(
            Xr, Yr, 
            C = z$C, epsilon = z$epsilon,
            kernel = fkern
            )
        
        # Same as
        #res <- kernlab::ksvm(
        #    Xr, Yr, 
        #    C = z$C, epsilon = z$epsilon,
        #    kernel = do.call(zkern, zkpar)
        #    )
        
        zfm <- list(y = Yu, fit = predict(res, Xu))
        if(is.numeric(zfm$y) & is.numeric(zfm$fit))
            zfm$r <- zfm$y - zfm$fit
        else
            zfm$r <- rep(NA, m)
        
        z <- length(zfm$y) 
        dat <- data.frame(
            matrix(rep(unlist(param[i, ]), z), ncol = npar, byrow = TRUE),
            seq_len(m),
            rownam.Xu
            )
        names(dat) <- c(names(param), "rownum", "rownam")
        
        y[[i]] <- cbind(dat, y1 = zfm$y)
        fit[[i]] <- cbind(dat, y1 = zfm$fit)
        r[[i]] <- cbind(dat, y1 = zfm$r)        
        
        }
    
    if(print) cat("\n\n")
    
    y <- setDF(rbindlist(y))
    fit <- setDF(rbindlist(fit))
    r <- setDF(rbindlist(r))    
        
    list(y = y, fit = fit, r = r)

    }


