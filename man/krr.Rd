\name{krr}
\alias{krr}
\encoding{latin1}

\title{Non Linear Kernel Ridge Regression}

\description{

Function \code{krr} fits non linear kernel ridge regression models. 

The kernel Gram matrix is internally centered before the analyses, but the data are not column-wise scaled. If needed, the user has to do the scaling before using the function  (there is no argument \code{scale} in the function). 

Row observations can eventually be weighted (using argument \code{weights}).

}

\usage{

krr(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, kern = kpol,
  weights = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

\item{lambda}{A scalar or numeric vector defining the regularization parameter value(s).}

\item{unit}{Unit used for lambda (Default to \code{unit = 1}). For instance, \code{lambda = 12, unit = 1e-6, ...} corresponds to a value \code{lambda = 12e-6}.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}


\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}
}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{tr}{The trace of the hat matrix (estimated df).}

}

\references{

Cule, E., De Iorio, M., 2012. A semi-automatic method to guide the choice of ridge parameter in ridge regression. arXiv:1205.0686.

Hastie, T., Tibshirani, R., 2004. Efficient quadratic regularization for expression arrays. Biostatistics 5, 329-340. https://doi.org/10.1093/biostatistics/kxh010

Hastie, T., Tibshirani, R., Friedman, J., 2009. The elements of statistical learning: data mining, inference, and prediction, 2nd ed. Springer, New York.

Hoerl, A.E., Kennard, R.W., 1970. Ridge Regression: Biased Estimation for Nonorthogonal Problems. Technometrics 12, 55-67. https://doi.org/10.1080/00401706.1970.10488634

}

\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X[1:8, ] ; Yr <- Y[1:8, ] 
Xu <- X[9:10, ] ; Yu <- Y[9:10, ] 

fm <- krr(Xr, Yr, Xu, Yu, lambda = c(.1, .2), degree = 3)
## Same as:
## fm <- krr(Xr, Yr, Xu, Yu, lambda = c(1, 2), unit = .1, degree = 3)

fm$y
fm$fit
fm$r

mse(fm, ~ lambda + unit)
mse(fm, ~ lambda + unit, nam = "y2")

}

\keyword{datagen}