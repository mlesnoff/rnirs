\name{mavg}
\alias{mavg}
\encoding{latin1}

\title{Smoothing by moving average}

\description{Smoothing, by moving average, of the row observations (usually spectra) of a data set.}

\usage{mavg(X, n = 3)}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame (usually spectra) on which are calculated the moving averages.}

\item{n}{The number of points (i.e. columns of \code{X}) defining the window over wich is calculate each average. The smoothing is calculated for the point at the center of the window. Therefore, \code{n} must be an odd integer, and be higher or equal to 3.}

}

\value{

A matrix.

}


\examples{

data(datcass)

X <- datcass$Xu

n <- 11
zX <- mavg(X, n) 

headm(X)
headm(zX)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(zX, main = "Corrected signal")
par(oldpar)

}

\keyword{datagen}