influ.pls <- function(fm, ncomp = NULL) {
  
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  if(is.null(ncomp))
    ncomp <- ncol(fm$Tr)
  
  weights <- fm$weights
  n <- length(weights)

  ymeans <- fm$ymeans
  Ymeans <- matrix(rep(ymeans, n), nrow = n, byrow = TRUE)
  
  q <- length(ymeans)

  U <- diag(sqrt(weights))

  T <- fm$Tr[, 1:ncomp, drop = FALSE]
  B <- t(fm$C)[1:ncomp, , drop = FALSE]
  
  fit <- Ymeans + T %*% B
  
  e <- fm$Y - fit 

  Te <- cbind(rep(1, n), T)
  zTe <- U %*% Te
  zY <- U %*% fm$Y
  ze <- U %*% e

  k <- ncol(Te)
  s <- sqrt(colSums(ze^2) / (n - k))
  
  h <- hat(zTe, intercept = FALSE)
  
  mh <- matrix(rep(h, q), ncol = q)
  ms <- matrix(rep(s, n), nrow = n, byrow = TRUE)
  
  # Internally Studentized residuals
  # = diag(1 / sqrt(1 - h)) %*% ze %*% diag(1 / s)
  r <- ze / (ms * sqrt(1 - mh))
  
  # PRESS residuals
  ze.press <- ze / (1 - mh)

  cook <- (1 / k) * mh / (1 - mh) * r^2 
  
  row.names(cook) <- row.names(ze.press) <- row.names(r) <- row.names(e)

  
  list(r = e, rstud.int = r, r.press = ze.press, cook = cook)

  }