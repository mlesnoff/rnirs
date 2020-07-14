\name{kpls.nipals}
\alias{kpls.nipals}
\encoding{latin1}

\title{Non linear kernel PLS algorithm}

\description{

Function \code{kpls.nipals} fits a NIPALS KPLS (Rosipal & Trejo, 2001). 

The kernel Gram matrice is internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function  . 

Row observations can eventually be weighted (using argument \code{weights}).

}

\usage{

kpls.nipals(X, Y, ncomp, kern = kpol, weights = NULL,
                         tol = .Machine$double.eps^0.5, maxit = 100, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of observations.}

\item{Y}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of responses. }

\item{ncomp}{The number of scores (= components = latent variables) to consider.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax, and other available kernel functions.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{tol}{Tolerance level for stopping the NIPALS iterations.}

\item{maxit}{Maximum number of NIPALS iterations.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}

}

\value{

A list of outputs (see examples), such as:

\item{T}{KPLS scores (\eqn{n x ncomp}).}

}

\references{

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

}

\examples{

n <- 8
p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

kpls.nipals(X, Y, ncomp = 5)

}

\keyword{datagen}