\name{dalm}
\alias{dalm}
\encoding{latin1}

\title{DA using multivariate linear regression}

\description{

Discriminant analysis using multivariate linear regression.

The class membership (unidimensional variable) \eqn{y} for the reference (= training) observations is firstly transformed (with function \code{\link{dummy}}) to a table \eqn{Ydummy} containing \eqn{nclas} dummy variables, where \eqn{nclas} is the number of classes in \eqn{y}. Then, a multivariate linear regression (eventually weighted)  is fitted over the \eqn{X}-data and the dummy table \eqn{Ydummy}. For a given new observation to predict, the final predicted class corresponds to the dummy variable (i.e. column of the dummy table \eqn{Ydummy}) for which the prediction is the highest.

When the number of classes is higher than two, this method  can be affected by a masking effect (see eg. Hastie et al. 2009, section 4.2): some class(es) can be masked (therefore not well predicted) if more than two classes are aligned in the \eqn{X}-space. Caution should be taken about such eventual masking effects.

When argument \code{weights = NULL} (default), \code{dalm} is strictly equivalent to \code{daglm(..., family = gaussian)} but faster.

}

\usage{
dalm(Xr, Yr, Xu, Yu = NULL, weights = NULL)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{weights}{A vector of length \eqn{n} defining the statistical weights to apply to the training observations. Defalut to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{

Hastie, T., Tibshirani, R., Friedman, J., 2009. 2nd Ed. The elements of statistical learning. Data mining, inference and prediction. Springer.

}

\examples{

data(iris)

X <- iris[, 1:4]
y <- iris[, 5]
N <- nrow(X)

m <- round(.25 * N) 
n <- N - m        
s <- sample(1:N, m)
Xr <- X[-s, ]
yr <- y[-s]
Xu <- X[s, ]
yu <- y[s]

fm <- dalm(Xr, yr, Xu, yu)
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
head(fm$dummyfit)
fm$ni
err(fm)

}

\keyword{datagen}