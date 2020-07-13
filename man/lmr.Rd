\name{lmr}
\alias{lmr}
\encoding{latin1}

\title{Multiple Linear Regression Models}

\description{

Function \code{lmr} fits a multiple linear regression model (MLR) using function \code{\link{lm}}. 

Row observations can eventually be weighted.

}

\usage{

lmr(Xr, Yr, Xu, Yu = NULL, weights = NULL)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{fm}{Output of function \code{\link{lm}}.}

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

fm <- lmr(Xr, Yr, Xu, Yu)
names(fm)
fm$y
fm$fit
fm$r
names(fm$fm)

coef(fm$fm)

mse(fm, nam = "y1")
mse(fm, nam = "y2")
mse(fm)

}

\keyword{datagen}