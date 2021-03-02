\name{xinterp}
\alias{xinterp}
\encoding{latin1}

\title{Resampling of spectra by nterpolation methods}

\description{

Resampling of spectra (or any signal) by interpolation methods, including linear, spline, and cubic interpolation. Uses \code{\link{interpl1}} of package \code{signal}.

}

\usage{

xinterp(X, w, meth = "cubic", ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of spectra (or more generally signals). The names of the columns of \code{X} are taken as the wavelengths (or more generally x-values), \eqn{w0}. If these names are missing or \code{as.numeric(colnames(X))} contains NAs, they are automatically set to \code{w0 = 1:p}.}

\item{w}{A vector of the values where to interpolate (typically within the range of \eqn{w0}).}

\item{meth}{The method of interpolation. See \code{\link{interpl1}}.}

\item{...}{Optionnal arguments to pass in function \code{\link{splinefun}} if \code{meth = "spline"}.}

}

\value{
A matrix of the interpolated spectra.
}


\examples{

data(datcass)

X <- datcass$Xu
w <- seq(500, 2400, length = 10)
zX <- xinterp(X, w, meth = "spline")
plotsp(zX)

}

\keyword{datagen}