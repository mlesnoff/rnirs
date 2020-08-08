\name{daglm}
\alias{daglm}
\encoding{latin1}

\title{DA using GLIM Regression on the Y-Dummy table}

\description{

DA-GLM

1- The class membership \eqn{y} (unidimensional variable) for the reference (= training) observations is firstly transformed (with function \code{\link{dummy}}) to a table \eqn{Ydummy} containing a number of \eqn{nclas} dummy variables, where \eqn{nclas} is the number of classes in \eqn{y}. 

2- Then, a generalized linear regression model (GLIM, using function \code{\link{glm}}) is fitted between the \eqn{X}-data and each of the dummy variables (i.e. columns of the dummy table \eqn{Ydummy}). 

3- For a given new observation, the final prediction (a class) corresponds to the dummy variable for which the prediction is the highest.

When the number of classes is higher than two, this method  can be affected by a masking effect (see eg. Hastie et al. 2009, section 4.2): some class(es) can be masked (therefore not well predicted) if more than two classes are aligned in the \eqn{X}-space. Caution should thereefore be taken about such eventual masking effects.

}

\usage{
daglm(Xr, Yr, Xu, Yu = NULL, family = binomial(link = "logit"))
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{family}{Specify the GLIM model used by function \code{\link{glm}}. See function \code{\link{family}}. By default, a logistic binomial regression model is fitted.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

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

## Binomial model with logit link (logistic regression)

fm <- daglm(Xr, yr, Xu, yu)
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)
fm$ni

err(fm)

## Gaussian model with identity link (= usual linear model)

fm <- daglm(Xr, yr, Xu, yu, family = gaussian)
err(fm)

}

\keyword{datagen}