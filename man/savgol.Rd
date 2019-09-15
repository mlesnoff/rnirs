\name{savgol}
\alias{savgol}
\encoding{latin1}

\title{Savitzky-Golay smoothing}

\description{
Smoothing, with a Savitzky-Golay filter, of the row observations (usually spectra) of a data set .
}

\usage{savgol(X, m, n, p, ts = 1, ranges = NULL)}

\arguments{

\item{X}{A matrix or data frame. The column names of \eqn{X} must be integers (usually the wavelengths).}

\item{n}{A filter length (must be odd), i.e. number of colums in matrix \eqn{X} that defines the calculation window.}

\item{p}{A polynomial order.}

\item{m}{A derivation order.}

\item{ts}{A scaling factor (e.g. the step between two wavelengths in matrix \eqn{X}), see argument \code{ts} in function \code{\link{sgolayfilt}}. This has not impact on the form of the output.}

\item{ranges}{See \code{\link{selw}}. Default to \code{NULL}: all the columns of \code{X} are considered. If not \code{NULL}, the transformation is implemented separately for each range.}

}

\details{\code{savgol} uses functions \code{\link{sgolayfilt}} and \code{\link{sgolay}} of package \code{signal}.}


\value{A matrix of the transformed data.}

\examples{
data(datcass)

X <- datcass$Xr

z <- savgol(X, m = 1, n = 11, p = 3)
plotsp(z)

z <- savgol(X, m = 1, n = 11, p = 3, ts = 2)
plotsp(z)

# Filtering and and then selection of ranges

z <- savgol(X, m = 1, n = 11, p = 3)
ranges <- list(c(1000, 1700), c(1800, 2500))
z <- selw(z, ranges)$X
plotsp(z, ranges = ranges)

# Selection of ranges and then filtering within ranges

z <- savgol(X, m = 1, n = 11, p = 3, ranges = ranges)
plotsp(z, ranges = ranges)
}

\keyword{datagen}