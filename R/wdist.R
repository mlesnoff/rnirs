wdist <- function(d, h = 2, cri = 3, square = FALSE) {
    
    d <- c(d)
    if(square)
        d <- d^2

    w <- ifelse(d <    median(d) + cri * mad(d), exp(-d / (h * mad(d))), 0)
    w <- w / max(w)
    
    w[is.na(w) | is.nan(w)] <- 1
        
    w
    
    }





