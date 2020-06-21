\name{pca.svd}
\alias{pca.svd}
\alias{pca.eigen}
\alias{pca.nipals}
\alias{pca.nipalsna}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms for usual (centered) PCA of a matrix \eqn{X}.

- \code{pca.svd}: SVD decomposition, using function \code{\link{svd}}. 

- \code{pca.eigen}: eigen decomposition, using function \code{\link{eigen}}. 

- \code{pca.nipals}: NIPALS. 

- \code{pca.nipalsna}: NIPALS allowing missing data in \eqn{X} (\code{NA} are not allowed in the other algorithms). The core of \code{pca.nipals} uses the code of function \code{nipals} of package \code{nipals} v.0.7 (Thanks to K. Wright, 2020) available on CRAN.

\bold{Argument} \code{weights}

Functions above (except \code{pca.nipalsna}) can give a priori weights to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores and loadings. For instance, function \code{pca.svd} implements a SVD of \eqn{D^(1/2) X}, where D = diag(weights) and X has been centered with metric \eqn{D}. Function \code{pca.eignen} implements an eigen decomposition of \eqn{X' D X}.

}

\usage{

pca.svd(X, ncomp, weights = NULL, kern = FALSE)

pca.eigen(X, ncomp, weights = NULL, kern = FALSE)

pca.nipals(X, ncomp, weights = NULL,
  tol = .Machine$double.eps^0.5, maxit = 100)

pca.nipalsna(X, ncomp, gramschmidt = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 100)
  
}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{kern}{For \code{pca.svd} and \code{pca.eigen}. If  \code{TRUE} (default is \code{FALSE}), the kernel versions are used (see Wu et al. 1997). This can be faster when \eqn{n < p}. In particular, for wide matrices (\eqn{n << p}) and \eqn{n} not too large, it is recommended using  \code{eigen(..., kernel = TRUE)}.}

\bold{Specific arguments of} \code{pca.nipalsna}

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

\item{eig}{The eigenvalues (\code{= sv^2}; vector of length \eqn{ncomp}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

}

\references{

Gabriel, R. K., 2002. Le biplot - Outil d'exploration de données multidimensionnelles. Journal de la Société Française de la Statistique, 143, 5-55.

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wright, K., 2018. Package nipals: Principal Components Analysis using NIPALS with Gram-Schmidt Orthogonalization. https://cran.r-project.org/

Wu, W., Massart, D.L., de Jong, S., 1997. The kernel PCA algorithms for wide data. Part I: Theory and algorithms. Chemometrics and Intelligent Laboratory Systems 36, 165-172. https://doi.org/10.1016/S0169-7439(97)00010-5

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