\name{lmr}
\alias{lmr}
\encoding{latin1}

\title{PLSR models}

\description{

Function \code{lmr} fits a linear regression model (LMR) using function \code{\link{lm}}. The row observations can eventually be weighted.

}

\usage{

lmr(Xr, Yr, Xu, Yu = NULL, weights = NULL, stor = FALSE)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{weights}{A vector of length \eqn{n} defining the statistical weights to apply to the observations. Default to \code{NULL} (weights = \eqn{1/n}).}

\item{stor}{Logical (default to \code{FALSE}). If \code{TRUE}, the function stores all the outputs of function \code{\link{lm}} and  auxilliary statistics  (e.g. b-coefficients), in a sub-object \code{fm}.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{fm}{Outputs of \code{\link{lm}} and other auxilliary statistics. \code{NULL} if \code{stor = FALSE}.}

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

fm <- lmr(Xr, Yr, Xu, Yu, stor = TRUE)
names(fm)

fm$fm$coef
fm$fit

mse(fm, nam = "y1")
mse(fm, nam = "y2")
mse(fm)

}

\keyword{datagen}