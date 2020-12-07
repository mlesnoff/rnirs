\name{pca.svd}
\alias{pca.svd}
\alias{pca.eigen}
\alias{pca.eigenk}
\alias{pca.nipals}
\alias{pca.nipalsna}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms for centered PCA of a matrix \eqn{X}. 

Noting \eqn{D} a diagonal matrix of row weights:

- \code{pca.svd}: SVD decomposition of \eqn{D^(1/2) * X}, using function \code{\link{svd}}. 

- \code{pca.eigen}: eigen decomposition of \eqn{X' * D * X}, using function \code{\link{eigen}}. 

- \code{pca.eigenk}: eigen decomposition of \eqn{D^(1/2) * X * X' D^(1/2)}, using function \code{\link{eigen}}. This is the "kernel cross-product trick" version of the PCA algorithm (Wu et al. 1997). For wide matrices (\eqn{n << p}) and \eqn{n} not too large, this algorithm can be much faster than the others. 

- \code{pca.nipals}: eigen decomposition of \eqn{X' * D * X} using NIPALS. 

- \code{pca.nipalsna}: eigen decomposition of \eqn{X' * D * X} using a NIPALS version allowing missing data in \eqn{X}. The core of \code{pca.nipalsna} uses the code of function \code{nipals} of package \code{nipals} v.0.7 (Thanks to K. Wright, 2020) available on CRAN.

Data are internally centered before the analyses, but they are not column-wise scaled (there is no argument \code{scale} available). If needed, the user has to do the scaling before using the functions. 
The functions (except \code{pca.nipalsna}) can set a priori weights to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores and loadings. Below, \eqn{D} is the diagonal matrix of the weights. 

\code{NA} are not allowed in the functions except \code{pca.nipalsna}.

}

\usage{

pca.svd(X, ncomp, weights = NULL)

pca.eigen(X, ncomp, weights = NULL)

pca.eigenk(X, ncomp, weights = NULL)

pca.nipals(X, ncomp, weights = NULL,
  tol = .Machine$double.eps^0.5, maxit = 100)

pca.nipalsna(X, ncomp, gs = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 200)
  
}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\bold{Specific arguments of} \code{pca.nipalsna}

\item{gs}{Logical. If TRUE (default), when there are missing data, a Gram-Schmidt orthogonalization is implemented at each iteration of the NIPALS algorithm. This slightly corrects the scores to insure that they are orthogonal. See vignettes in package \code{nipals}.}

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