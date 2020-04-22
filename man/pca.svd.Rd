\name{pca.svd}
\alias{pca.svd}
\alias{pca.eigen}
\alias{pca.nipals}
\alias{pca.nipalsna}
\alias{pca.sph}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms for PCA of a matrix \eqn{X}.

- \code{pca.svd}: SVD decomposition, using function \code{\link{svd}}. 

- \code{pca.eigen}: eigen decomposition, using function \code{\link{eigen}}. 

- \code{pca.nipals}: NIPALS. 

- \code{pca.nipalsna}: NIPALS allowing missing data in \eqn{X} (\code{NA} are not allowed in the other algorithms). The core of \code{pca.nipals} uses the optimized code (for computation time) of function \code{nipals} of package \code{nipals} (K. Wright) available on CRAN.

- \code{pca.sph}: Spherical PCA (Locantore et al. 1990). The spatial median is calculated with the code of rrcov v.1.4-3 on R CRAN (V. Todorov, 2016).

All algorithms center matrix \eqn{X}.

The functions (except \code{pca.nipalsna}) can give a priori weights to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores and loadings. For instance, function \code{pca.svd} implements a SVD of \eqn{D^(1/2) X}, where D = diag(weights) and X has been centered with metric \eqn{D}. Function \code{pca.eignenw} implements an eigen decomposition of \eqn{X' D X}.

}

\usage{

pca.svd(X, ncomp, weights = NULL)

pca.eigen(X, ncomp, weights = NULL)

pca.nipals(X, ncomp, weights = NULL,
  tol = .Machine$double.eps^0.5, maxit = 100)

pca.nipalsna(X, ncomp, gramschmidt = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 100)
  
pca.sph(X, ncomp, weights = NULL)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

Specific arguments of \code{pca.nipalsna}:

\item{gramschmidt}{Logical. If TRUE (default), when there are missing data, a Gram-Schmidt orthogonalization is implemented at each iteration of the NIPALS algorithm. This slightly corrects the scores to insure that they are orthogonal. See vignettes in package \code{nipals}.}

\item{tol}{Tolerance for testing convergence of the NIPALS iterations for each principal component.}

\item{maxit}{Maximum number of NIPALS iterations for each principal component.}

}

\value{

A list of outputs, such as:

\item{T}{The score matrix (\eqn{n x ncomp}).}

\item{P}{The loadings matrix (\eqn{p x ncomp}).}

\item{R}{The projection matrix (= \eqn{P} ; \eqn{p x ncomp}).}

\item{sv}{The singular values (vector of length \eqn{ncomp}).}

\item{xss}{The eigenvalues (\code{sv^2}, vector of length \eqn{ncomp}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

}

\references{

Gabriel, R. K., 2002. Le biplot - Outil d'exploration de données multidimensionnelles. Journal de la Société Française de la Statistique, 143, 5-55.

N. Locantore, J.S. Marron, D.G. Simpson, N. Tripoli, J.T. Zhang, K.L. Cohen, Robust principal component analysis for functional data, Test 8 (1999) 1–7

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

######## WITH MISSING DATA

X2 <- X
X2[3, 3] <- X2[1, 3] <- X2[1, 2] <- NA
X2

fm <- pca.nipalsna(X2, ncomp = 3)
fm

## Replacement of the missing data in X2
## by their NIPALS estimates

Xhat <- xfit(fm$T, fm$P, fm$xmeans)
Xhat
u <- which(is.na(X2))
Xfull <- replace(X2, u, Xhat[u])
Xfull

}

\keyword{datagen}