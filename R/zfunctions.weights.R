.bisquare <- function(x, a = 1)
  c(ifelse(abs(x) < a, (1 - (x / a)^2)^2, 0))

.cauchy <- function(x, a = 1)
  c(ifelse(abs(x) < a, 1 / (1 + (x / a)^2), 0))

.epan <- function(x, a = 1)
  c(ifelse(abs(x) < a, 1 - (x / a)^2, 0))

.fair <- function(x, a = 1)
  c(ifelse(abs(x) < a, 1 / (1 + abs(x / a))^2, 0))

.gauss <- function(x, a = 1)
  c(ifelse(abs(x) < a, exp(-(x / a)^2), 0))

.huber <- function(x, a = 1.345)
  c(ifelse(abs(x) < a, 1, a / abs(x)))

.invexp <- function(x, a = 1)
  c(ifelse(abs(x) < a, 1 / exp(abs(x / a)), 0))

.talworth <- function(x, a = 1)
  c(ifelse(abs(x) < a, 1, 0))

.trian <- function(x, a = 1)
  c(ifelse(abs(x) < a, 1 - abs(x / a), 0))

.tricube <- function(x, a = 1)
  c(ifelse(abs(x) < a, (1 - abs(x / a)^3)^3, 0))

############## KERNELS

.krbf <- function(X, Y = NULL, sigma = 1) {
  exp(-.5 * .dist(X, Y) / sigma^2)
  }

.kpoly <- function(X, Y = NULL, degree = 1, scale = 1, offset = 0) {
  
  if(is.null(Y))
    K <- (scale * tcrossprod(X) + offset)
  else
    K <- (scale * tcrossprod(X, Y) + offset)
  
  if(degree > 1) {
    zK <- K
    for(i in 1:(degree - 1))
      K <- K * zK
    }
  
  K  
    
  }









