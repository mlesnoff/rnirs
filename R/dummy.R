dummy <- function(Y) {
  
    if(sum(is.na(Y)) > 0)  stop("NA in 'Y' are not allowed")
  
    Y <- as.factor(Y)
    # levels returns the sorted character level names 
    lev <- levels(Y)
    nlev <- length(lev)
  
    #if(nlev > nlevmax)  
    #  stop("Too many levels. Check if 'Y' is really a qualitative variable.")
  
    if(nlev == 1) 
        Y <- factor(Y, levels = c(lev, ".NA"))

    z <- model.matrix(~ Y - 1)
    attr(z, "assign") <- NULL
    attr(z, "contrasts") <- NULL
  
    colnames(z) <- substring(colnames(z), first = 2, last = 1000000L)

    z
  
    }

