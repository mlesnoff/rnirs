kgram <- function(Xr, Xu = NULL, kern = kpol, ...) {
  
  Xr <- .matrix(Xr)
  
  Kr <- kern(Xr, ...)
  
  Ku <- NULL
  if(!is.null(Xu))
    Ku <- kern(.matrix(Xu), Xr, ...)

  list(Kr = Kr, Ku = Ku)

  }

