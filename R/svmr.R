svmr <- function(Xr, Yr, Xu, Yu = NULL,
                 C = 1, epsilon = .1, kern = c("poly", "rbf", "tanh"), 
                 print = TRUE, ...) {
  
  kern <- match.arg(kern)
  
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
  
  if(kern == "poly") {
    zkern <- polydot
    if(is.null(dots$degree)) dots$degree <- 1
    if(is.null(dots$scale)) dots$scale <- 1
    if(is.null(dots$offset)) dots$offset <- 1
    kpar <- list(degree = dots$degree, scale = dots$scale, offset = dots$offset)
    }
  if(kern == "rbf") {
    zkern <- rbfdot
    if(is.null(dots$sigma)) dots$sigma <- 1
    kpar <- list(sigma = dots$sigma)
    }
  if(kern == "tanh") {
    zkern <- tanhdot
    if(is.null(dots$scale)) dots$scale <- 1
    if(is.null(dots$offset)) dots$offset <- 0
    kpar <- list(scale = dots$scale, offset = dots$offset)
    }
  kpar <- lapply(kpar, FUN = function(x) sort(unique(x)))
  
  listpar <- c(list(C = C, epsilon = epsilon), kpar) 
  param <- expand.grid(listpar)
  npar <- ncol(param)

  r <- fit <- y <- vector(mode = "list", length = npar)
  
  if(print){
    cat(paste("\n Kernel: ", kern, "\n", sep = ""))
    cat("\n Parameters: \n")
    print(listpar)
    cat(paste("\n Nb combinations: ", nrow(param), "\n\n", sep = ""))
    }
  
  r <- fit <- y <- vector(mode = "list", length = npar)

  for(i in 1:nrow(param)) {
    
    z <- param[i, ]
    
    if(print)
      print(z)
      #cat(i, "")
    
    if("sigma" %in% names(z))
      z$sigma <- 1 / (2 * z$sigma^2)
    ### See ?kernlab::rbfdot
    ### sigma <- 17 ; krbf(1:10, 2:11, sigma = sigma) ; rbfdot(1 / (2 * sigma^2))(1:10, 2:11)
    ### sigma.rbfdot = 1 / (2 * sigma.krbf^2)
    
    zkpar <- z[, -(1:2), drop = FALSE]
    fkern <- do.call(zkern, zkpar)

    res <- kernlab::ksvm(
      Xr, Yr, 
      C = z$C, epsilon = z$epsilon,
      kernel = fkern
      )
    
    # Same as:
    #res <- kernlab::ksvm(
    #  Xr, Yr, 
    #  C = z$C, epsilon = z$epsilon,
    #  kernel = do.call(zkern, zkpar)
    #  )
    
    zfm <- list(y = Yu, fit = predict(res, Xu))
    if(is.numeric(zfm$y) & is.numeric(zfm$fit))
      zfm$r <- zfm$y - zfm$fit
    else
      zfm$r <- rep(NA, m)
    
    z <- length(zfm$y) 
    dat <- data.frame(
      matrix(rep(unlist(param[i, ]), z), ncol = npar, byrow = TRUE),
      1:m,
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


