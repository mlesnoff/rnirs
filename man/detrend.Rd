\name{detrend}
\alias{detrend}
\encoding{latin1}

\title{Detrend transformation}

\description{

Detrend transformations of the row observations (usually spectra) of a data set. 

- Method \code{poly} fits an orthogonal polynom (of a given degree). 

- Method \code{lowess} fits a LOWESS smoother (Cleveland, 1979) with function \code{\link{lowess}}.

- Method \code{als} fits an asymmetric least square (ALS) regression model (Eilers & Boelens, 2005). The function uses function \code{\link{asysm}} of package \code{\link{ptw}} (J. Gerretzen, P. Eilers, H. Wouters, T. Bloemberg & R. Wehrens).  

The function returns either the residuals (corrected spectra) or the fitted predictions (baselines) depending on argument \code{baseline}.


}

\usage{detrend(X, method = c("poly", "lowess", "als"), ...)}

\arguments{

\item{X}{A matrix or data frame. The column names of \eqn{X} must be integers (usually wavelengths).}

\item{method}{The detrend method.}

\item{...}{Other arguments to pass depending on the method. See the examples.}

}

\details{\code{detrend} uses function \code{\link{poly}} of package \code{stat}.}

\value{A matrix of the transformed data.}

\references{

- Cleveland, W. S. (1979). Robust locally weighted regression and smoothing scatterplots. Journal of the American Statistical Association, 74, 829-836. doi: 10.1080/01621459.1979.10481038.

- Eilers, P. H. C., Boelens, H. F. M., 2005. Baseline correction with asymmetric least squares smoothing

}

\value{

A matrix.

}



\examples{

data(datcass)

X <- datcass$Xu

headm(X)

### Polynom

degree <- 1
#degree <- 2
z <- detrend(X, method = "poly", degree = degree)
headm(z)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(z, zeroes = TRUE, main = "Corrected signal")
plotsp(X - z, main = "Baseline")
par(oldpar)

### Lowess

z <- detrend(X, method = "lowess", f = .5)
headm(z)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(z, zeroes = TRUE, main = "Corrected signal")
plotsp(X - z, main = "Baseline")
par(oldpar)

### ALS

lambda <- 1e7 ; p <- .05
z <- detrend(X, method = "als", lambda = lambda, p = p)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(z, zeroes = TRUE, main = "Corrected signal")
plotsp(X - z, main = "Baseline")
par(oldpar)

}

\keyword{datagen}