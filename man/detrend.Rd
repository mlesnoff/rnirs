\name{detrend}
\alias{detrend}
\encoding{latin1}

\title{Detrend transformation}

\description{
Detrend transformation of the row observations (usually spectra) of a data set. An orthogonal polynom (of a given degree) is fitted for each observation. The function returns the residuals.
}

\usage{detrend(X, degree = 1, ranges = NULL)}

\arguments{

\item{X}{A matrix or data frame. The column names of \eqn{X} must be integers (usually wavelengths).}

\item{degree}{The degree of the polynomial.}

\item{ranges}{See \code{\link{selw}}. Default to \code{NULL}: all the columns of \code{X} are considered. If not \code{NULL}, the transformation is implemented separately for each range.}

}

\details{\code{detrend} uses function \code{\link{poly}} of package \code{stat}.}

\value{A matrix of the transformed data.}

\examples{
data(datcass)

X <- datcass$Xr

par(mfrow = c(1, 2))
z <- detrend(X)
plotsp(z, main = "Linear detrend")
z <- detrend(X, degree = 2)
plotsp(z, main = "Quadratic detrend")
par(mfrow = c(1, 1))

# Detrend and then selection of ranges

z <- detrend(X)
ranges <- list(c(1000, 1700), c(1800, 2500))
z <- selw(z, ranges)
plotsp(z$X, ranges = ranges)

# Selection of ranges and then detrend within ranges

z <- detrend(X, ranges = ranges)
plotsp(z, ranges = ranges)
}

\keyword{datagen}