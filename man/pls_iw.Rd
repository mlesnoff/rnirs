\name{pls_iw}
\alias{pls_iw}
\encoding{latin1}

\title{Robust PLS1 algorithm (iterative Re-Weighting)}

\description{

A robust PLS1 algorithm using iterave reweighted PLS. The weights of the row observations are iteratively calculated from a X-outlyingness measure (computed in a PLS score space) and the PLS y-residuals.

This is a simplified and slightly modified version of the robuts PLSR algorithm "PRM" of Serneels et al. 2005. See the related article for details, and the code of  \code{pls_iw} which is very comprehensive. 

Compared to PRM, two modifications are proposed in \code{pls_iw}:

- The weights of the row observations are optimzed for a single given number of components, \eqn{ncompw}, set by the user. In PRM, the weights are optimized for each number of PLS components which is very time expensive (and which generates as many models as the total number of PLS components considered).

- The weight function is the tricubic function such as in Cleveland & Devlin (1988). 

Data are internally centered before the analyses, but they are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function. 

Row observations can receive additionnal a priori weights (using argument \code{weights}).

}

\usage{

pls_iw(X, Y, ncomp, ncompw = 10, a = 3, 
                     tol = 1e-2, maxit = 10, weights = NULL)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{A \eqn{n x 1} matrix or data frame, or vector of length \eqn{n}, of responses.}

\item{ncomp}{The maximal number of scores (components = latent variables) to be calculated in the final PLS.}

\item{ncompw}{The number of scores used for optimizing the WPLS weights.}

\item{a}{The cutoff value for the tricubic weight function (\code{.tricube}).}

\item{tol}{Tolerance value for the iterative weights optimization.}

\item{maxit}{Maximum number of iterations for the weights optimization.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

A list of outputs, such as:

\item{T}{The X-score matrix (\eqn{n x ncomp}).}

\item{P}{The X-loadings matrix (\eqn{p x ncomp}).}

\item{W}{The X-loading weights matrix (\eqn{p x ncomp}).}

\item{C}{The Y-loading weights matrix (C = t(Beta), where Beta is the scores regression coefficients matrix).}

\item{R}{The PLS projection matrix (\eqn{p x ncomp}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

\item{ymeans}{The centering vector of \eqn{Y} (length \eqn{q}).}

}

\references{

Cleveland, W.S., Devlin, S.J., 1988. Locally Weighted Regression: An Approach to Regression Analysis by Local Fitting. Journal of the American Statistical Association 83, 596–610. https://doi.org/10.1080/01621459.1988.10478639

Serneels, S., Croux, C., Filzmoser, P., Van Espen, P.J., 2005. Partial robust M-regression. Chemometrics and Intelligent Laboratory Systems 79, 55-64. https://doi.org/10.1016/j.chemolab.2005.04.007

}

\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y <- 100 * rnorm(n)
set.seed(NULL)

pls_iw(X, y, ncomp = 3)

fm <- plsr(X, y, X, ncomp = 3, algo = pls_iw, ncompw = 2)

}

\keyword{datagen}