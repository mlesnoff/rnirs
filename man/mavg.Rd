\name{mavg}
\alias{mavg}
\encoding{latin1}

\title{Smoothing by moving average}

\description{Smoothing, by moving average, of the row observations (usually spectra) of a data set.}

\usage{mavg(X, n = 3)}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame (usually spectra) on which are calculated the moving averages.}

\item{n}{The number of points (i.e. columns of \code{X}) defining the window over wich is calculate each average. The smoothing is calculated for the point at the center of the window. Therefore, \code{n} must be an odd integer (and be higher or equal to 3).}

}

\examples{

data(datcass)

X <- datcass$Xr
X[1:5, 1:5]

z <- mavg(X, n = 5)
z[1:5, 1:5]
plotsp(z[sample(1:nrow(z), 100), ])

}

\keyword{datagen}