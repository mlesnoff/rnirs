\name{pca_svd}
\alias{pca_svd}
\alias{pca_eigen}
\alias{pca_eigenk}
\alias{pca_nipals}
\alias{pca_nipalsna}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms implementing a centered PCA of a matrix \eqn{X}. 

Except for \code{pca_nipalsna}, a priori weights can be set to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores and loadings. By default, argument \code{weights} is set to \code{NULL} corresponding to the usual weights \eqn{1/n}.

Noting \eqn{D} a \eqn{n x n} diagonal matrix of weights for the observations (rows of \eqn{X}), the functions consist in:

- \code{pca_svd} ==> SVD decomposition of \eqn{D^(1/2) * X}, using function \code{\link{svd}}. 

- \code{pca_eigen} ==> eigen decomposition of \eqn{X' * D * X}, using function \code{\link{eigen}}. 

- \code{pca_eigenk} ==> eigen decomposition of \eqn{D^(1/2) * X * X' D^(1/2)}, using function \code{\link{eigen}}. This is the "kernel cross-product trick" version of the PCA algorithm (Wu et al. 1997). For wide matrices (\eqn{n << p}) and \eqn{n} not too large, this algorithm can be much faster than the others. 

- \code{pca_nipals} ==> eigen decomposition of \eqn{X' * D * X} using NIPALS. 

- \code{pca_nipalsna} ==> eigen decomposition of \eqn{X' * D * X} using NIPALS allowing missing data in \eqn{X}. 

In all the functions, matrix \eqn{X} is centered before the analyses, but \eqn{X} is not column-wise scaled (there is no argument \code{scale} available). If a scaling is needed, the user has to scale \eqn{X} before using the functions. 

Function \code{pca_nipalsna} accepts missing data (\code{NA}s) in \eqn{X}, unlike the other functions. The part of \code{pca_nipalsna} accounting specifically for missing missing data in the NIPALS algorithm is based on the efficient code of K. Wright in the R package \code{nipals} (https://cran.r-project.org/web/packages/nipals/index.html).

\bold{Gram-Schmidt orthogonalization in the NIPALS algorithm}

The PCA NIPALS is known generating a loss of orthogonality of the scores (due to the accumulation of rounding errors in the successive iterations), particularly for large matrices or with high degrees of column collinearity.

With missing data, orthogonality of loadings is not satisfied neither.

A performant approach for coming back to orthogonality (scores and loadings) is the iterative classical Gram-Schmidt orthogonalization (Lingen 2000, Andrecut 2009, and vignette of R package \code{nipals}), referred to as the iterative CGS. It consists in adding a CGS orthorgonalization step in each iteration of the scores and loadings calculations.

For the case with missing data, the iterative CGS does not insure that the orthogonalized scores are centered.

}

\usage{

pca_svd(X, ncomp, weights = NULL)

pca_eigen(X, ncomp, weights = NULL)

pca_eigenk(X, ncomp, weights = NULL)

pca_nipals(X, ncomp, weights = NULL,
    gs = TRUE, 
    tol = .Machine$double.eps^0.5, maxit = 200)

pca_nipalsna(X, ncomp, 
  gs = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 200)
  
}

\arguments{

\item{X}{A matrix or dataframe (\eqn{n x p}).}

\item{ncomp}{The maximal number of PCA scores (= components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\bold{Specific arguments for the NIPALS} 

\item{gs}{For \code{pca_nipalsna} and \code{pca_nipalsna}. Logical indicating if a Gram-Schmidt orthogonalization is implemented or not (default to \code{TRUE}).}

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

\item{niter}{Numbers of iterations of the NIPALS.}

\item{conv}{Logical indicating if the NIPALS converged before reaching the maximal number of iterations.}

}

\references{

Andrecut, M., 2009. Parallel GPU Implementation of Iterative PCA Algorithms. Journal of Computational Biology 16, 1593-1599. https://doi.org/10.1089/cmb.2008.0221

Gabriel, R. K., 2002. Le biplot - Outil d'exploration de données multidimensionnelles. Journal de la Société Française de la Statistique, 143, 5-55.

Lingen, F.J., 2000. Efficient Gram–Schmidt orthonormalisation on parallel computers. Communications in Numerical Methods in Engineering 16, 57-66. https://doi.org/10.1002/(SICI)1099-0887(200001)16:1<57::AID-CNM320>3.0.CO;2-I

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wright, K., 2018. Package nipals: Principal Components Analysis using NIPALS with Gram-Schmidt Orthogonalization. https://cran.r-project.org/

Wu, W., Massart, D.L., de Jong, S., 1997. The kernel PCA algorithms for wide data. Part I: Theory and algorithms. Chemometrics and Intelligent Laboratory Systems 36, 165-172. https://doi.org/10.1016/S0169-7439(97)00010-5

}

\examples{

n <- 6
p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), nrow = n,)
set.seed(NULL)
s <- c(3, 4, 7, 10, 11, 15, 21:24)   
zX <- replace(X, s, NA)
X
zX

pca_svd(X, ncomp = 3)

pca_eigen(X, ncomp = 3)

pca_nipals(X, ncomp = 3)

pca_nipalsna(X, ncomp = 3)

}

\keyword{datagen}