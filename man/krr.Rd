\name{krr}
\alias{krr}
\encoding{latin1}

\title{Non Linear Kernel Ridge Regression}

\description{

Function \code{krr} fits KRR models, also referred to as LS-SVM (Suykens et al. 2000, Bennett & Embrechts 2003, Krell 2018). 

The kernel Gram matrices \eqn{K} are internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function.

Row observations can eventually be weighted with a priori weights (using argument \code{weights}).

\bold{Note:} An alternative to \code{krr}  is to run a "direct" kernel approach (e.g. Bennett & Embrechts 2003), i.e. to build preliminary kernel Gram matrices (such as doing a pre-processing on \eqn{X}), and then run usual RR on them. See examples in function \code{\link{kgram}}.

See also the tuning facility with \code{\link{splitpar}}.

}

\usage{

krr(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{lambda}{A value, or vector of values, of the regularization parameter \eqn{lambda}.}

\item{unit}{A scalar. Unit used for lambda (Default to \code{unit = 1}). For instance, \code{lambda = 12, unit = 1e-6, ...} corresponds to a value \code{lambda = 12e-6}.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{print}{Logical (default = \code{TRUE}). If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}. The value set in the kernel parameters (e.g. \code{degree} for \code{\link{kpol}})  can be a scalar or a vector of several values.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{tr}{The trace of the hat matrix (estimated df).}

}

\references{

Bennett, K.P., Embrechts, M.J., 2003. An optimization perspective on kernel partial least squares regression, in: Advances in Learning Theory: Methods, Models and Applications, NATO Science Series III: Computer & Systems Sciences. IOS Press Amsterdam, pp. 227-250.

Cawley, G.C., Talbot, N.L.C., 2002. Reduced Rank Kernel Ridge Regression. Neural Processing Letters 16, 293-302. https://doi.org/10.1023/A:1021798002258

Krell, M.M., 2018. Generalizing, Decoding, and Optimizing Support Vector Machine Classification. arXiv:1801.04929.

Saunders, C., Gammerman, A., Vovk, V., 1998. Ridge Regression Learning Algorithm in Dual Variables, in: In Proceedings of the 15th International Conference on Machine Learning. Morgan Kaufmann, pp. 515–521.

Suykens, J.A.K., Lukas, L., Vandewalle, J., 2000. Sparse approximation using least squares support vector machines. 2000 IEEE International Symposium on Circuits and Systems. Emerging Technologies for the 21st Century. Proceedings (IEEE Cat No.00CH36353). https://doi.org/10.1109/ISCAS.2000.856439

Welling, M., n.d. Kernel ridge regression. Department of Computer Science, University of Toronto, Toronto, Canada. https://www.ics.uci.edu/~welling/classnotes/papers_class/Kernel-Ridge.pdf

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

fm <- krr(Xr, Yr, Xu, Yu, lambda = c(.1, .2), degree = 2:3, offset = c(0, 1))
## Same as:
## fm <- krr(Xr, Yr, Xu, Yu, lambda = c(1, 2), unit = .1, degree = 2:3, offset = c(0, 1))

fm$y
fm$fit
fm$r

mse(fm, ~ lambda + unit + degree + scale + offset)
mse(fm, ~ lambda + unit + degree + scale + offset, nam = "y2")

}

\keyword{datagen}