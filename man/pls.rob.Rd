\name{pls.rob}
\alias{pls.rob}
\encoding{latin1}

\title{Robust PLS1 algorithm}

\description{

A robust PLS1 algorithm combining PCA outlyingness measures, PLS y-residuals and weighted PLS (WPLS). 

X- and y-outliers are detected in a PCA space and a PLS space, respectively, each one having a given number \eqn{ncompw} of components set by the user. These outliers receive a weight 0 in a final weighted PLS.   

In details, the three steps are:

- Step1: A robust PCA is implemented (using \code{\link{pca.rob}}) with \eqn{ncompw} components. The SD-OD outlyingness (\code{\link{out.sdod}}) computed on the robust score space are used for detecting X-outliers. The  \eqn{alpha * n} observations (where \eqn{alpha} is a proportion) that have the highest outlyingness receive a weight \eqn{wx = 0} (the other receive a weight \eqn{wx = 1}).

- Step2: A weighted PLS with \eqn{ncompw} components is implemented with weights \eqn{wx}. The y-residuals are robustly centered and scaled by median and MAD, respectively. Observations with residuals higher than a given cutoff (parametric or non parametric; argument \code{typcut}) received a weight \eqn{wy = 0} (the other receive a weight \eqn{wy = 1}).

- Step3: The final PLS is a weighted PLS with \eqn{ncomp} components and weights \eqn{wx * wy}. 

Data are internally centered before the analyses, but they are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function. 

Row observations can receive additionnal a priori weights (using argument \code{weights}).

}

\usage{

pls.rob(X, Y, ncomp, ncompw = 10, alpha = .30,
                    typcut = c("param", "mad"), weights = NULL, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{A \eqn{n x 1} matrix or data frame, or vector of length \eqn{n}, of responses.}

\item{ncomp}{The maximal number of scores (= components = latent variables) to be calculated in the final PLS.}

\item{ncompw}{The number of scores used for computing the X- and y-outliers (Steps 1 and 2).}

\item{alpha}{Proportion \eqn{alpha} of the data used as hard rejection of X-outliers in Step 1 (See \code{\link{pca.rob}}). Default to \code{alpha = .30}, i.e. 30pct are rejected.}


\item{typcut}{Type of cutoff used for the y-residuals (centered and scaled) in Step 2. Possible values are \code{"param"} (default; cutoff = -/+ .975 Gaussian quantile) or \code{"mad"} (cutoff = 2.5).}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{...}{Optional arguments to pass in function \code{\link{out.sdod}} used in \code{\link{pca.rob}}.}

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

\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y <- 100 * rnorm(n)
set.seed(NULL)

pls.rob(X, y, ncomp = 3)

fm <- plsr(X, y, X, ncomp = 3, algo = pls.rob, ncompw = 2)

}

\keyword{datagen}