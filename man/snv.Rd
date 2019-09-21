\name{snv}
\alias{snv}
\encoding{latin1}

\title{Standard normal variate transformation (SNV)}

\description{
SNV transformation of the row observations (usually spectra) of a data set. By default, each observation is centered on its mean and divided by its standard deviation.
}

\usage{snv(X, center = TRUE, scale = TRUE, ranges = NULL)}

\arguments{

\item{X}{A matrix or data frame. The column names of \eqn{X} must be integers (usually the wavelengths).}

\item{center}{Logical. If \code{TRUE} (default), the centering in the SNV is done.}

\item{scale}{Logical. If \code{TRUE} (default), the scaling in the SNV is done.}

\item{ranges}{See \code{\link{selw}}. Default to \code{NULL}: all the columns of \code{X} are considered. If not \code{NULL}, the transformation is implemented separately for each range.}

}

\value{A matrix of the transformed data.}

\examples{
data(datcass)

X <- datcass$Xr

z <- snv(X)
plotsp(z)

z <- snv(X)
ranges <- list(c(1000, 1700), c(1800, 2500))
z <- selw(z, ranges)$X
plotsp(z, ranges = ranges)

z <- snv(X, ranges = ranges)
plotsp(z, ranges = ranges)
}

\keyword{datagen}