\name{kplsda}
\alias{kplsda}
\alias{kplsdalm}
\alias{kpcda}
\alias{kpcdalm}
\alias{dkplsda}
\alias{dkplsdalm}
\encoding{latin1}

\title{Non linear kernel PCDA and PLSDA models}

\description{

KPCDA and KPLSDA 

Discrimination (DA) based on latent variables (scores) computed from KPCA (Scholkopf et al. 1997, Scholkopf & Smola 2002, Tipping 2001) or  KPLS (Rosipal & Trejo 2001).

Functions \code{kplsda} and \code{kplsdalm} do the same as \code{\link{plsda}} and \code{\link{plsdalm}}, respectively, but using KPLS scores returned by \code{\link{kpls}}.

Functions \code{kpcda} and \code{kpcdalm} do the same as \code{\link{pcda}} and \code{\link{pcdalm}}, respectively, but using KPCA scores returned by function \code{\link{kpca}}.

The kernel Gram matrices \eqn{K} are internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function.

Row observations can eventually be weighted with a priori weights (using argument \code{weights}).

DKPLSDA

The true kernel algorithms above are time expensive when \eqn{n > 500}, especially KPLSDA due to the iterative deflation of the \eqn{n x n} training Gram matrix \eqn{K}. A much faster alternative to KPLSDA is to run a "direct kernel PLSDA" (DKPLSDA) (Bennett & Embrechts 2003), i.e. to build preliminary kernel Gram matrices (such as doing a pre-processing on \eqn{X}), and then to run a usual PLSDA algorithm on them. This is what do functions \code{dkplsda} and  \code{dkplsdalm}. See also examples in function \code{\link{kgram}}.

See also the tuning facility with \code{\link{splitpar}}.

}

\usage{

kpcda(Xr, Yr, Xu, Yu = NULL, ncomp, da = dalm, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

kpcdalm(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

kplsda(Xr, Yr, Xu, Yu = NULL, ncomp, da = dalm, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

kplsdalm(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

dkplsda(Xr, Yr, Xu, Yu = NULL, ncomp, da = dalm, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

dkplsdalm(Xr, Yr, Xu, Yu = NULL, ncomp, 
                 kern = kpol, weights = NULL, print = TRUE, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{ncomp}{The number of scores (i.e. components) to consider.}

\item{da}{A function defining the discriminant method used for the predictions. Default to \code{\link{dalm}}.} 

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

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

data(datforages)
Xr <- datforages$Xr
yr <- datforages$yr
Xu <- datforages$Xu
yu <- datforages$yu
Xr <- detrend(Xr)
Xu <- detrend(Xu)
headm(Xr)
headm(Xu)
table(yr)
table(yu)

######## KPCDALM

ncomp <- 20
fm <- kpcda(Xr, yr, Xu, yu, ncomp = ncomp, kern = krbf, sigma = c(1, 10))
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)

z <- err(fm, ~ ncomp + sigma)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = z$sigma)

## Same with kpcdalm (faster)

ncomp <- 20
fm <- kpcdalm(Xr, yr, Xu, yu, ncomp = ncomp, kern = krbf, sigma = c(1, 10))
z <- err(fm, ~ ncomp + sigma)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = z$sigma)

######## KPCDA-PROB (QDA)

ncomp <- 20
fm <- kpcda(Xr, yr, Xu, yu, ncomp, da = daprob, degree = 2:3, lda = FALSE)
z <- err(fm, ~ ncomp + degree)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = z$degree)

}

\keyword{datagen}