\name{savgol}
\alias{savgol}
\encoding{latin1}

\title{Savitzky-Golay smoothing}

\description{

Smoothing by derivation, with a Savitzky-Golay filter, of the row observations (usually spectra) of a data set .

The function uses function \code{\link{sgolayfilt}}.

}

\usage{savgol(X, m, n, p, ts = 1)}

\arguments{

\item{X}{A matrix or data frame. The column names of \eqn{X} must be integers (usually the wavelengths).}

\item{n}{A filter length (must be odd), i.e. number of colums in matrix \eqn{X} that defines the calculation window.}

\item{p}{A polynomial order.}

\item{m}{A derivation order.}

\item{ts}{A scaling factor (e.g. the step between two wavelengths in matrix \eqn{X}), see argument \code{ts} in function \code{\link{sgolayfilt}}. This has not impact on the form of the output.}

}

\details{\code{savgol} uses functions \code{\link{sgolayfilt}} and \code{\link{sgolay}} of package \code{signal}.}


\value{A matrix of the transformed data.}

\examples{

X <- datcass$Xu

m <- 1 ; n <- 11 ; p <- 3
z <- savgol(X, m, n, p)

headm(X)
headm(z)

par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(z, zeroes = TRUE, main = "Corrected signal")
par(mfrow = c(1, 1))

}

\keyword{datagen}