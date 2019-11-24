\name{snv}
\alias{snv}
\encoding{latin1}

\title{Standard normal variate transformation (SNV)}

\description{
SNV transformation of the row observations (usually spectra) of a data set. By default, each observation is centered on its mean and divided by its standard deviation.
}

\usage{snv(X, center = TRUE, scale = TRUE)}

\arguments{

\item{X}{A matrix or data frame. The column names of \eqn{X} must be integers (usually the wavelengths).}

\item{center}{Logical. If \code{TRUE} (default), the centering in the SNV is done.}

\item{scale}{Logical. If \code{TRUE} (default), the scaling in the SNV is done.}

}

\value{A matrix of the transformed data.}

\examples{

data(datcass)

X <- datcass$Xu

center <- TRUE
#center <- FALSE
par(mfrow = c(1, 2))
z <- X
headm(z)
plotsp(z, main = "Signal")
z <- snv(X, center = center)
headm(z)
plotsp(z, main = "Corrected signal")
par(mfrow = c(1, 1))

}

\keyword{datagen}