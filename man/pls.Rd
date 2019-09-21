\name{pls}
\alias{pls}
\alias{pca}
\encoding{latin1}

\title{PCA or PLS}

\description{

Calculation of PCA or PLS (1 or 2) scores and loadings for training and test data sets.

\code{pca} and \code{pls} use the denominator \eqn{n - 1} (not \eqn{n}) in the variance, covariance and correlation calculations.

}

\usage{
pca(Xr, Xu = NULL, ncomp, algo = pca.svd, ...)

pls(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The number of PCA or PLS scores (i.e. components) to be calculated.}

\item{algo}{A function (algorithm) implementing a PCA (default to \code{\link{pca.svd}}) or a PLS (default to \code{\link{pls.kernel}}).}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of outputs, such as:

\item{Tr}{The Xr-score matrix (\eqn{n x ncomp}).}

\item{Tu}{The Xu-score matrix (\eqn{m x ncomp}).}

\item{P}{The Xr-loadings matrix (\eqn{p x ncomp}).}

\item{explvar}{Proportions of \eqn{Xr-}variance explained by the Xr-score space.}

And other outputs: See the function defined in \code{algo}.

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
fm <- pca(Xr, ncomp = ncomp)
names(fm)
fm$Tr   
fm$P

fm <- pca(Xr, Xu, ncomp = ncomp)
fm$Tr
fm$Tu

ncomp <- 3
fm <- pls(Xr, Yr, ncomp = ncomp)
names(fm)
fm$Tr
fm$P

fm <- pls(Xr, Yr, Xu, ncomp = ncomp)
fm$Tr
fm$Tu

pls(Xr, Yr, Xu, ncomp = ncomp, algo = pls.kernelw, 
  weights = rep(1, nrow(Xr)))$Tu

}









\keyword{datagen}