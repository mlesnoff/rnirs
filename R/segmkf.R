segmkf <- function(n, y = NULL, K = 5, type = c("random", "consecutive", "interleaved"), 
                   nrep = 1, seed = NULL) {
  
  type <- match.arg(type)

  segm <- vector("list", length = nrep)
  names(segm) <- paste("rep", 1:nrep, sep = "")
  
  n <- round(n)
  
  zn <- n
  if(!is.null(y)) {
    if(length(y) != n) stop("y must be of size n")
    yagg <- unique(y)
    zn <- length(yagg)
    }

  lseg <- ceiling(zn / K)
  nna <- K * lseg - zn
  
  set.seed(seed = seed)

  for(i in seq_len(nrep)) {
  
    z <- switch(type,
      random = matrix(c(sample(1:zn), rep(NA, nna)), ncol = K, byrow = TRUE),
      consecutive = {
        x <- c(matrix(c(rep(1, zn), rep(NA, nna)), ncol = K, byrow = TRUE))
        x[!is.na(x)] <- cumsum(na.omit(x))
        x <- matrix(x, ncol = K, byrow = FALSE)
        x
        },
      interleaved = matrix(c(1:zn, rep(NA, nna)), ncol = K, byrow = TRUE)
      )
    z <- lapply(data.frame(z), FUN = function(x) c(na.omit(x)))
    names(z) <- paste("segm", 1:K, sep = "")
    
    segm[[i]] <- z
    
    }
  
  if(!is.null(y)) {
    vecn <- seq_len(n)
    zsegm <- segm
    for(i in 1:nrep) {
      for(j in seq_len(K)){
        u <- segm[[i]][[j]]
        v <- which(y %in% yagg[u])
        zsegm[[i]][[j]] <- v
        }
      }
    segm <- zsegm  
    }
  
  set.seed(seed = NULL)

  segm
  
  }
