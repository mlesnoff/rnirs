\name{fitcv}
\alias{fitcv}
\encoding{latin1}

\title{Cross-validation of a prediction model}

\description{

\code{fitcv} is a generic function for implementing cross-validation of a prediction model. Argument \code{fun} defines the function implementing the model has to be cross-validated.

}

\usage{

fitcv(X, Y, fun, segm, print = FALSE, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of reference (= training) row observations.}

\item{Y}{For quantive responses: A \eqn{n x q} matrix or data frame, or vector of length \eqn{n}, of reference (= training) responses. For qualitative responses: A vector of length \eqn{n}, or \eqn{n x 1} matrix,  of reference (= training) responses (class membership).}

\item{fun}{A function defining the prediction model to cross-validate.}

\item{segm}{A list of the test segments. Typically, output of function \code{\link{segmcvkfold}} or \code{\link{segmcvmc}}.} 

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass through function \code{fun}.}

}

\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
dim(Xr)

n <- nrow(Xr)
segm <- segmcvkfold(n = n, K = 5, typ = "random", nrep = 3)   
#segm <- segmcvmc(n = n, m = 40, nrep = 30)                   
fm <- fitcv(
  Xr, yr,
  fun = plsr,
  ncomp = 20,
  segm = segm,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)

z <- mse(fm, ~ ncomp)
head(z)
z[z$rmsep == min(z$rmsep), ]

z <- mse(fm, ~ ncomp + rep)
head(z)
plotmse(z, group = "rep")

plotmse(z, nam = "r2", group = "rep")

}

\keyword{datagen}