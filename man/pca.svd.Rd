\name{pca.svd}
\alias{pca.svd}
\alias{pca.eigen}
\alias{pca.nipals}
\alias{pca.svdw}
\alias{pca.eigenw}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms for PCA.

\code{pca.svd} implements a SVD decomposition, using function \code{\link{svd}}. 

\code{pca.eigen} implements an eigen decomposition, using function \code{\link{eigen}}. 

\code{pca.nipals} implements the NIPALS PCA algorithm. The core of \code{pca.nipals} uses the optimized (for computation time) code  of function \code{nipals} of package \code{nipals} (K. Wright) available on CRAN. Missing data are allowed.

Functions \code{pca.eigenw} and \code{pca.svdw} allows to define statistical weights for the observations different from the weights \eqn{1/n} used in standard PCA. This modifies the importance of the \eqn{n} training observations in the calculations of the scores and loadings.

Missing values are not allowed except for function \code{pca.nipals}.
}

\usage{

pca.svd(X, ncomp)

pca.eigen(X, ncomp)

pca.nipals(X, ncomp, gramschmidt = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 100)

pca.svdw(X, ncomp, weights)

pca.eigenw(X, ncomp, weights)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining the statistical weights to apply to the observations. }

Specific arguments of \code{pca.nipals}:

\item{gramschmidt}{Logical. If TRUE (default), when there are missing data, a Gram-Schmidt orthogonalization is implemented at each iteration of the NIPALS algorithm. This slightly corrects the scores to insure that they are orthogonal. See vignettes in package \code{nipals}.}

\item{tol}{Tolerance for testing convergence of the NIPALS iterations for each principal component.}

\item{maxit}{Maximum number of NIPALS iterations for each principal component.}

}


\details{

The functions center \eqn{X} before the analysis, and normalize the eventual statistical weights to sum to 1 (the \code{weights} input is automatically tranformed to \code{weights/sum(weights)}). If all the weights are set to be equal, this is the standard PCA.

}

\value{

A list of outputs, such as:

\item{T}{The score matrix (\eqn{n x ncomp}).}

\item{P}{The loadings matrix (\eqn{p x ncomp}).}

\item{R}{The projection matrix (= \eqn{P} ; \eqn{p x ncomp}).}

\item{sv}{The vector of the singular values (length \eqn{n} or \eqn{p}).}

\item{xss}{The square of the norms of the scores (columns of \eqn{T}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

}

\references{

Gabriel, R. K., 2002. Le biplot - Outil d'exploration de données multidimensionnelles. Journal de la Société Française de la Statistique, 143, 5-55.

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wright, K., 2018. Package nipals: Principal Components Analysis using NIPALS with Gram-Schmidt Orthogonalization. https://cran.r-project.org/

}


\examples{

n <- 6
p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(NULL)
X

pca.svd(X, ncomp = 3)
pca.eigen(X, ncomp = 3)
pca.nipals(X, ncomp = 3)

pca.svdw(X, ncomp = 3, weights = rep(1, n))

}

\keyword{datagen}