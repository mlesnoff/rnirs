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

\item{n}{The number of points (i.e. columns of \code{X}) defining the window over wich is calculate each finite difference. The derivation is calculated for the point at the center of the window. Therefore, \code{n} must be an odd integer, and be higher or equal to 3.}

\item{ts}{A scaling factor for the finite differences (by default, \code{ts} = 1.)}
}

\value{

A matrix.

}


\examples{

data(datcass)

X <- datcass$Xu

n <- 11
zX <- dderiv(X, n)                 # = 1st derivate
#zX <- dderiv(dderiv(X, n), n)     # = 2nd derivate

headm(X)
headm(zX)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(zX, main = "Corrected signal", zeroes = TRUE)
par(oldpar)

}

\keyword{datagen}