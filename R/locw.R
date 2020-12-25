locw <- function(
    Xr = NULL, Yr,
    Xu = NULL, Yu = NULL,
    listnn,
    listw = NULL,
    fun,
    stor = TRUE,
    print = TRUE,
    ...
    ) {
    
    dots <- list(...)

    if("print" %in% names(formals(fun)))
        dots$print <- FALSE

    fun <- match.fun(fun)
    
    Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
    n <- nrow(Yr)
    q <- ncol(Yr)
    colnam.Yr <- colnames(Yr)
    
    m <- length(listnn)
    if(is.null(Yu)) Yu <- matrix(nrow = m, ncol = q)
        else Yu <- .matrix(Yu, row = FALSE, prefix.colnam = "y")
    
    if(!is.null(Xr)) Xr <- .matrix(Xr) else Xr <- Yr
    if(!is.null(Xu)) Xu <- .matrix(Xu) else Xu <- Yu
    rownam.Xu <- row.names(Xu)
    
    weighted <- !is.null(listw)
    fm <- y <- r <- fit <- vector("list", length = m)
    names(fm) <- seq_len(m) 
    for(i in seq_len(m)) {
        
        if(print)
            if(m <= 220) cat(i, "") else if(i %in% seq(1, m, by = 5)) cat(i, "")
        
        ind <- listnn[[i]]
        
        #if(length(ind == 0)) next
        
        if(weighted) dots$weights <- listw[[i]]
        param <- c(
            list(
                Xr = Xr[ind, , drop = FALSE], 
                Yr = Yr[ind, , drop = FALSE], 
                Xu = Xu[i, , drop = FALSE], 
                Yu = Yu[i, , drop = FALSE]), 
            dots
            )
        fm[[i]] <- do.call(fun, param)
        
        fm[[i]]$nn <- ind
        
        nr <- nrow(fm[[i]]$y)
        fm[[i]]$r$rownum <- fm[[i]]$fit$rownum <- fm[[i]]$y$rownum <- rep(i, nr)
        fm[[i]]$r$rownam <- fm[[i]]$fit$rownam <- fm[[i]]$y$rownam <- rep(rownam.Xu[i], nr)
        
        k <-    rep(length(ind), nr)

        y[[i]] <- data.frame(k = k, fm[[i]]$y)
        fit[[i]] <- data.frame(k = k, fm[[i]]$fit)
        r[[i]] <- data.frame(k = k, fm[[i]]$r)
        
        }

    if(print) cat("\n\n")

    .f <- function(x) {
        x <- lapply(x, function(x) data.frame(x))
        x <- setDF(rbindlist(x))
        }
    y <- .f(y)
    fit <- .f(fit)
    r <- .f(r)
    
    if(!stor)
        fm <- NULL
    
    rm(list = c("param"))
    gc()
    
    list(y = y, fit = fit, r = r, fm = fm)
    
    }
    
    