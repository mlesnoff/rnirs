detrend <- function(X, method = c("poly", "lowess", "als"), ...) {

  X <- .matrix(X)

  fun <- switch(
    match.arg(method),
    "poly" = .detrend.poly,
    "lowess" = .detrend.lowess,
    "als" = .detrend.als
    )

  X <- fun(X, ...)
  
  X
  
  }



