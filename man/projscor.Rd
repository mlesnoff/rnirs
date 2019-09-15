\name{projscor}
\alias{projscor}
\encoding{latin1}

\title{Projection on a score space}

\description{

Projection of row observations of a data set on a calculated score (e.g. PCA or PLS) space.

}

\usage{projscor(X, fm)}

\arguments{

\item{X}{A \eqn{m x p} matrix or data frame of observations to project.}

\item{fm}{An object containing the score space information. Typically, output of PCA or PLS functions.}

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

ncomp <- 3
fm <- pca.svd(Xr, ncomp = ncomp)
projscor(Xu, fm)
pca(Xr, Xu, ncomp = ncomp)$Tu

ncomp <- 3
fm <- pls.kernel(Xr, Yr, ncomp = ncomp)
projscor(Xu, fm)
pls(Xr, Yr, Xu, ncomp = ncomp)$Tu

}

\keyword{datagen}