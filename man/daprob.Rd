\name{daprob}
\alias{daprob}
\encoding{latin1}

\title{Probabilistic DA (LDA and QDA)}

\description{

For each observation to predict, the function calculates the posterior probability that the observation belongs to a given class, using the Bayes' formula. For each of the classes, the posterior probability is computed from given priors (proportional or uniform) for the class membership and an estimate (parametric or not) of the probability density at the point of the observation conditionnally to the class. The final predicted class corresponds to the class with the highest posterior probability.

}

\usage{

daprob(Xr, Yr, Xu, Yu = NULL, dens = dmnorm, 
  lda = TRUE, prior = c("proportional", "uniform"), ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{dens}{A function returning the probability density of the observation conditionnally to the class. Default to \code{\link{dmnorm}}.}

\item{lda}{Logical, only considered if \code{dens = dmnorm}. If \code{TRUE} (default), a gaussian LDA is implemented, otherwise a gaussian QDA is implemented.}

\item{prior}{The prior probabilities of class membership. Possible values are "proportional" (default; probablities are set equal to the observed proportions of the classes in \code{Yr}), "uniform" (probablities are set equal for all the classes), or a vector of values defining the probabilities for each class.}

\item{...}{Optionnal arguments to pass in function defined in \code{dens}.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{

Saporta, G., 2011. Probabilités analyse des données et statistique. Editions Technip, Paris, France.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.

}

\examples{

data(iris)

X <- iris[, 1:4]
y <- iris[, 5]
N <- nrow(X)

m <- round(.25 * N) # Test
n <- N - m          # Training
s <- sample(1:N, m)
Xr <- X[-s, ]
yr <- y[-s]
Xu <- X[s, ]
yu <- y[s]

##### LDA (homogeneous covariances)

fm <- daprob(Xr, yr, Xu, yu, dens = dmnorm)
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)
fm$ni

err(fm)

##### QDA (heterogeneous covariances)

fm <- daprob(Xr, yr, Xu, yu, dens = dmnorm, lda = FALSE)
err(fm)

##### Nonparametric DA

fm <- daprob(Xr, yr, Xu, yu, dens = dkern.gauss, h = .2)
err(fm)

}

\keyword{datagen}