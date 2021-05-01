sampclas <- function(x, y = NULL, k, seed = NULL) {
    
    x <- as.integer(c(x))
    
    n <- length(x)
    ni <- tabulate(as.factor(x))
    nclas <- length(ni)
    lev <- sort(unique(x))

    if(length(k) == 1) 
        k <- rep(k, nclas)
    else
        if(length(k) != nclas)
            stop("\n\n    Length of argument 'k' must be =1 
                 or =the number of classes in vector'x'. \n\n") 
    
    s <- list()
    set.seed(seed = seed) 
    for(i in seq_len(nclas)) {
        
        k[i] <- min(k[i], ni[i])
        
        zs <- which(x == lev[i])
    
        if(is.null(y)) {
            s[[i]] <- sample(zs, size = k[i], replace = FALSE)
            } 
        else {
            zy <- y[zs]
            ## "order(y) = 3 7 etc." means that the 1st lower value is the component 3 of the vector,
            ## the 2nd lower value is the component 7 of the vector,
            id <- order(zy)
            u <- round(seq(1, ni[i], length = k[i]))
            s[[i]] <- zs[id[u]]
            }
    
        }
    set.seed(seed = NULL)
    
    s <- unlist(s)
    
    list(train = s, test = seq_len(n)[-s])
    
    }
