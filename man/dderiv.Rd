\name{dderiv}
\alias{dderiv}
\encoding{latin1}

\title{Derivation by finite difference}

\description{
Calculation of the first derivatives, by finite differences, of the row observations (usually spectra) of a data set. 
}

\usage{dderiv(X, n = 3, ts = 1)}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame on which are calculated the derivatives.}

\item{n}{The number of points (i.e. columns of \code{X}) defining the window over wich is calculate each finite difference. The derivation is calculated for the point at the center of the window. Therefore, \code{n} must be an odd integer (and be higher or equal to 3).}

\item{ts}{A scaling factor for the finite differences (by default, \code{ts} = 1.)}
}

\examples{

data(datcass)

X <- datcass$Xr
X[1:5, 1:5]

# 1rst derivation
z <- dderiv(X, n = 5)
z[1:5, 1:5]
plotsp(z[sample(1:nrow(z), 100), ])

# 2nd derivation
z <- dderiv(dderiv(X, n = 5), n = 5)
z[1:5, 1:5]
plotsp(z[sample(1:nrow(z), 100), ])

# 1rst derivation and smoothing bu movering average
z <- mavg(dderiv(X, n = 5), n = 5)
z[1:5, 1:5]
plotsp(z[sample(1:nrow(z), 100), ])

}

\keyword{datagen}