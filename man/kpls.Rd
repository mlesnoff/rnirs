\name{kpls}
\alias{kpls}
\alias{kpca}
\encoding{latin1}

\title{Non Linear Kernel PLS and PCA}

\description{

KPLS (Rosipal & Trejo 2001) or KPCA (Scholkopf et al. 1997, Scholkopf & Smola 2002, Tipping 2001).

The kernel Gram matrices \eqn{K} are internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function.

Row observations can eventually be weighted with a priori weights (using argument \code{weights}).

\bold{Note:} These kernel algorithms are time expensive when \eqn{n > 500}, especially KPLS due to the iterative deflation of the \eqn{n x n} training Gram matrix \eqn{K}. A much faster alternative is to run "direct" kernel models, i.e. build preliminary kernel Gram matrices such as doing a pre-processing on \eqn{X} (e.g. Bennett & Embrechts 2003) and then run usual algorithms on them. See examples in function \code{\link{kgram}}.  

}

\usage{

kpls(Xr, Yr, Xu = NULL, ncomp, kern = kpol, weights = NULL, ...)

kpca(Xr, Xu = NULL, ncomp, kern = kpol, weights = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{For \code{kpls}. A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}

}

\value{

A list of outputs (see examples), such as:

\item{Tr}{The training score matrix in the feature space (\eqn{n x ncomp}).}

\item{Tu}{The test score matrix in the feature space (\eqn{m x ncomp}).}

\item{explvar}{For \code{kpca}. Eigen-values and proportions of variance explained in the feature space by the training scores.}

}

\references{

Bennett, K.P., Embrechts, M.J., 2003. An optimization perspective on kernel partial least squares regression, in: Advances in Learning Theory: Methods, Models and Applications, NATO Science Series III: Computer & Systems Sciences. IOS Press Amsterdam, pp. 227-250.

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

Scholkopf, B., Smola, A., Müller, K.-R., 1997. Kernel principal component analysis, in: Gerstner, W., Germond, A., Hasler, M., Nicoud, J.-D. (Eds.), Artificial Neural Networks — ICANN 97, Lecture Notes in Computer Science. Springer, Berlin, Heidelberg, pp. 583-588. https://doi.org/10.1007/BFb0020217

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

Tipping, M.E., 2001. Sparse kernel principal component analysis. Advances in neural information processing systems, MIT Press. http://papers.nips.cc/paper/1791-sparse-kernel-principal-component-analysis.pdf

}

\examples{

n <- 8
p <- 3
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
Xu <- X[1:2, ]
set.seed(NULL)

ncomp <- 4
kpls(X, Y, Xu, ncomp = ncomp, degree = 2)
kpca(X, ncomp = ncomp, kern = krbf, sigma = .5)

}

\keyword{datagen}