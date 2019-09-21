\name{daglm}
\alias{daglm}
\encoding{latin1}

\title{DA using generalized linear regression}

\description{

Discriminant analysis using generalized linear regression (GLIM).

The class membership (unidimensional variable) \eqn{y} for the reference (= training) observations is firstly transformed (with function \code{\link{dummy}}) to a table \eqn{Ydummy} containing \eqn{nclas} dummy variables, where \eqn{nclas} is the number of classes in \eqn{y}. Then, a GLIM model (using function \code{\link{glm}}) is fitted independently on the \eqn{X-}data and each of the \eqn{nclas} dummy variables of \eqn{Ydummy}. The \eqn{nclas} models provide \eqn{nclas} prediction vectors (one for each of the dummy variables) (using function \code{\link{predict.glm}}) for the new (= test) observations to predict. For a given new observation to predict, the final predicted class corresponds to the dummy variable (i.e. column of the dummy table \eqn{Ydummy}) for which the prediction is the highest.

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

m <- round(.25 * N) 
n <- N - m        
s <- sample(1:N, m)
Xr <- X[-s, ]
yr <- y[-s]
Xu <- X[s, ]
yu <- y[s]

fm <- daglm(Xr, yr, Xu, yu)
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
fm$ni
err(fm)

fm <- daglm(Xr, yr, Xu, yu, family = gaussian)
err(fm)

}

\keyword{datagen}