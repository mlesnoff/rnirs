selsign <- function(
    fm, formula = ~ 1, 
    nam = NULL,
    type = c("sign", "wilcox", "perm"), 
    nperm = 50, seed = NULL
    ) {
    
    type <- match.arg(type)
    
    if(is.null(nam))
         nam <- names(fm$y)[ncol(fm$y)]
    
    nrep <- max(fm$y$rep)
    
    vars <-    all.vars(formula)
    
    if(length(vars) == 1) {
        param <- data.frame(unique(fm$y[, vars]))
        names(param) <- vars
        } 
    else
        param <- expand.grid(lapply(fm$y[, vars], unique))
    
    PVAL <- matrix(nrow = nrep, ncol = nrow(param))
    for(i in seq_len(nrep)) {
        
        u <- (fm$y$rep == i)
        zfm <- list(y = fm$y[u, ], fit = fm$fit[u, ], r = fm$r[u, ])
    
        z <- mse(zfm, formula, digits = 20)
        zopt <- which(z$msep == min(z$msep))[1]
        zopt <- z[zopt, vars]
        
        res <- zfm$r

        r0 <- .subsetC(res, cols = vars, values = c(zopt))
        r0 <- r0[, nam]^2
        zn <- length(r0)
    
        pval <- numeric()
        for(j in seq_len(nrow(param))) {
            
            r <- .subsetC(res, cols = vars, values = c(param[j, ]))
            
            r <- r[, nam]^2
            
            if(type == "sign")
                PVAL[i, j] <- binom.test(
                    x = sum(r > r0),
                    n = zn, p = 0.5, 
                    alternative = "greater"
                    )$p.value
            
            if(type == "wilcox")
                PVAL[i, j] <- suppressWarnings(
                    wilcox.test(
                        r0, r, paired = TRUE, 
                        alternative = "less"
                        )$p.value
                    )
        
            if(type == "perm") {
                
                zd <- r - r0
                stat0 <- sum(zd)
                zd <- abs(zd)
                            
                stat <- list()
                set.seed(seed = seed)
                for(k in seq_len(nperm))
                    stat[k] <- sum(zd * sample(c(-1, 1), size = zn, replace = TRUE))
                set.seed(seed = NULL)
                    
                ## For two-sided
                ## Not useful here since stat0 
                ## is always >= 0 (it is opt)
                ## stat0 <- abs(stat0)
                ## stat <- abs(stat)
                ## End
                
                PVAL[i, j] <- sum(stat >= stat0) / nperm
                
                }

            }
        
        }
    
    z <- matrixStats::colMeans2(PVAL, na.rm = TRUE)
    pval <- data.frame(param, pval = z)
    
    list(pval = pval, PVAL = PVAL)
    
    }
