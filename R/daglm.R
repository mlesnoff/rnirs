daglm <- function(Xr, Yr, Xu, Yu = NULL, family = binomial(link = "logit"),
                                    weights = NULL){
    
    Xr <- .matrix(Xr)
    n <- dim(Xr)[1]
    
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
        dummyfit <- NULL
        }
    ### END
    
    else {
    
        if(is.null(weights))
            weights <- rep(1 / n, n)
        else
            weights <- weights / sum(weights) 
        
        Yrdummy <- dummy(Yr)
        
        z <- matrix(nrow = m, ncol = nclas)
        for(i in seq_len(nclas)) {
            
            dat <- data.frame(Yr = Yrdummy[, i], Xr)
            fm <- suppressWarnings(glm(Yr ~ ., family = family, data = dat, 
                                                                 weights = weights))
            #fm <- suppressWarnings(glm(Yr ~ ., family = family, data = dat))
            z[, i] <- predict(fm, newdata = data.frame(Xu), type = "response")
            
            }
        row.names(z) <- rownam.Xu
        colnames(z) <- lev
        dummyfit <- z
    
        z <- apply(dummyfit, FUN = .findmax, MARGIN = 1) 
        fit <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
 
        }

    y <- Yu
    r <- as.numeric(y != fit)
    
    y <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, y, stringsAsFactors = FALSE)
    fit <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, fit, stringsAsFactors = FALSE)
    r <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, r)
    names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Y

    list(y = y, fit = fit, r = r, dummyfit = dummyfit, ni = ni)
    
    }