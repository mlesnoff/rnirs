\name{kplsr}
\alias{kplsr}
\alias{kpcr}
\alias{dkplsr}
\encoding{latin1}

\title{Non linear kernel PLSR and PCR Models}

\description{

KPLSR, KPCR

Function \code{kplsr} fits KPLSR models with the NIPALS algorithm (implemented in \code{\link{kpls.nipals}}) such as described in Rosipal & Trejo (2001). 

Function \code{kpcr} fits a KPCR model, i.e. a regression on latent variables (scores) of KPCA  (Scholkopf et al. 1997, Scholkopf & Smola 2002, Tipping 2001), computed by \code{\link{kpca}}.

The kernel Gram matrices \eqn{K} are internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function.

Row observations can eventually be weighted with a priori weights (using argument \code{weights}).

DKPLSR

The true kernel algorithms above are time expensive when \eqn{n > 500}, especially KPLSR due to the iterative deflation of the \eqn{n x n} training Gram matrix \eqn{K}. A much faster alternative to KPLSR is to run a "direct kernel PLSR" (DKPLSR) (Bennett & Embrechts 2003), i.e. to build preliminary kernel Gram matrices (such as doing a pre-processing on \eqn{X}), and then to run a usual PLSR algorithm on them. This is what does function \code{dkplsr}. See also examples in function \code{\link{kgram}}.   

See also the tuning facility with \code{\link{splitpar}}.

}

\usage{

kplsr(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

kpcr(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

dkplsr(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of scores (= components = latent variables) to consider.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax, and other available kernel functions.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{print}{Logical (default = \code{TRUE}). If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}. The value set in the kernel parameters (e.g. \code{degree} for \code{\link{kpol}}) can be a scalar or a vector of several values.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{

Bennett, K.P., Embrechts, M.J., 2003. An optimization perspective on kernel partial least squares regression, in: Advances in Learning Theory: Methods, Models and Applications, NATO Science Series III: Computer & Systems Sciences. IOS Press Amsterdam, pp. 227-250.

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

Scholkopf, B., Smola, A., Müller, K.-R., 1997. Kernel principal component analysis, in: Gerstner, W., Germond, A., Hasler, M., Nicoud, J.-D. (Eds.), Artificial Neural Networks — ICANN 97, Lecture Notes in Computer Science. Springer, Berlin, Heidelberg, pp. 583-588. https://doi.org/10.1007/BFb0020217

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

Tipping, M.E., 2001. Sparse kernel principal component analysis. Advances in neural information processing systems, MIT Press. http://papers.nips.cc/paper/1791-sparse-kernel-principal-component-analysis.pdf

}

\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X[1:8, ] ; Yr <- Y[1:8, ] 
Xu <- X[9:10, ] ; Yu <- Y[9:10, ] 

ncomp <- 3
fm <- kpcr(Xr, Yr, Xu, Yu, ncomp = ncomp, degree = 3)
#fm <- kpcr(Xr, Yr, Xu, Yu, ncomp = ncomp, degree = 3)
names(fm)
z <- mse(fm, ~ ncomp)
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

## fictive weights
kplsr(Xr, Yr, Xu, Yu, ncomp = ncomp, weights = 1:nrow(Xr))
#dkplsr(Xr, Yr, Xu, Yu, ncomp = ncomp, weights = 1:nrow(Xr))   ## DKPLSR

####### Example of fitting the function sinc(x) (Rosipal & Trejo 2001 p. 105-106) 

x <- seq(-10, 10, by = .2)
x[x == 0] <- 1e-5
n <- length(x)
zy <- sin(abs(x)) / abs(x)
y <- zy + rnorm(n, 0, .2)
plot(x, y, type = "p")
lines(x, zy, lty = 2)
Xu <- Xr <- matrix(x, ncol = 1)

ncomp <- 3
fm <- kplsr(Xr, y, Xu, ncomp = ncomp, kern = krbf)
#fm <- kplsr(Xr, y, Xu, ncomp = ncomp, kern = krbf)   ## DKPLSR
fit <- fm$fit$y1[fm$fit$ncomp == ncomp]
plot(Xr, y, type = "p")
lines(Xr, zy, lty = 2)
lines(Xu, fit, col = "red")

}

\keyword{datagen}