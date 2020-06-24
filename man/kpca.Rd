\name{kpca}
\alias{kpca}
\encoding{latin1}

\title{KPCA}

\description{

Non-linear kernel PCA (e.g. Scholkopf et al. 1997, Scholkopf & Smola 2002, Tipping 2001).

The function can set a priori weights to the training observations (rows of \eqn{Xr}), with argument \code{weights}.

The kernel Gram matrix is internally centered before the analyses, but the data are not column-wise scaled. If needed, the user has to do the scaling before using the function  (there is no argument \code{scale} in the function).  

}

\usage{
kpca(Xr, Xu = NULL, ncomp, kern = kpol, weights = NULL, ...)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}).}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}

}

\value{

A list of outputs (see examples), such as:

\item{Tr}{The training score matrix in the feature space (\eqn{n x ncomp}).}

\item{Tu}{The test score matrix in the feature space (\eqn{m x ncomp}).}

\item{explvar}{Eigen-values and proportions of variance explained in the feature space by the training scores.}

}

\references{

Scholkopf, B., Smola, A., Müller, K.-R., 1997. Kernel principal component analysis, in: Gerstner, W., Germond, A., Hasler, M., Nicoud, J.-D. (Eds.), Artificial Neural Networks — ICANN’97, Lecture Notes in Computer Science. Springer, Berlin, Heidelberg, pp. 583-588. https://doi.org/10.1007/BFb0020217

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

Tipping, M.E., 2001. Sparse kernel principal component analysis. Advances in neural information processing systems, MIT Press. http://papers.nips.cc/paper/1791-sparse-kernel-principal-component-analysis.pdf

}

\examples{

n <- 5
p <- 4
m <- 3
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
Xu <- matrix(rnorm(m * p, mean = 10), ncol = p)
set.seed(NULL)

ncomp <- 3
kpca(X, ncomp = ncomp, degree = 2)
kpca(X, ncomp = ncomp, kern = krbf, sigma = .5)

kpca(X, Xu, ncomp = ncomp)

}

\keyword{datagen}