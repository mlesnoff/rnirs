matW <- function(X, y){
  
  X <- .matrix(X)
  n <- nrow(X)
  
  y <- as.factor(y)
  ni <- c(table(y))
  nclas <- length(ni)
  
  lev <- levels(y)
  namy <- as.character(lev)

  zy <- as.numeric(y)
  
  zp <- ni / n
  
  Wi <- vector(length = nclas, mode = "list")
  for(i in 1:nclas) {
  
    Wi[[i]] <- cov(X[which(zy == i), , drop = FALSE]) * (ni[i] - 1) / ni[i]
    
    colnames(Wi[[i]]) <- rownames(Wi[[i]])
    
    if(i == 1) W <- zp[i] * Wi[[i]] else W <- W + zp[i] * Wi[[i]]
    
    }
  names(Wi) <- namy
  
  ###### Case with class(es) with only 1 observation
  if(sum(ni == 1) > 0) {
    ind <- which(ni == 1)
    # give a covariance matrix to the class(es) with 1 observation
    sigma.1obs <- cov(X) * (n - 1) / n
    for(i in 1:length(ind)) Wi[[ind[i]]] <- sigma.1obs
    # Could give the weight 0 to the class(es) with 1 observation, as below:
    # zp <- ni ; zp[ind] <- 0 ; zp <- zp / sum(zp)
    for(i in 1:nclas) if(i == 1) W <- zp[i] * Wi[[i]] else W <- W + zp[i] * Wi[[i]]
    }
  ###### End

  list(Wi = Wi, W = W, ni = ni)
  
  }