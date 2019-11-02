\name{pls.kernel}
\alias{pls.kernel}
\alias{pls.kernelw}
\alias{pls.nipals}
\alias{pls.nipalsw}
\encoding{latin1}

\title{PLS algorithms}

\description{

PLS1 and PLS2 algorithms.

\code{pls.nipals} implements the NIPALS algorithm (e.g. Tenenhaus 1998, Wold 2002). 

\code{pls.kernel} implements the "improved kernel algorithm #1" proposed by Dayal and MacGregor (1997). This algorithm is stable and fast (Andersson 2009), and returns the same results as the NIPALS. 

\code{pls.nipalsw} and \code{pls.kernelw} are weighted versions of the two algorithms above, respectively. The particularity of the weighted PLS (WPLS) (e.g. Schaal et al. 2002, Sicard and Sabatier, 2006, Kim et al. 2011, Lesnoff et al., submitted) is to give different statistical weights (instead of \eqn{1/n} as in the standard PLS), and therefore importance, to the \eqn{n} training observations in the calculations of the scores, loadings and predictions.

Missing values are not allowed.

}

\usage{

pls.kernel(X, Y, ncomp)

pls.kernelw(X, Y, ncomp, weights = rep(1, nrow(X)))

pls.nipals(X, Y, ncomp)

pls.nipalsw(X, Y, ncomp, weights = rep(1, nrow(X)))

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{A \eqn{n x q} matrix or data frame (or vector of length \eqn{n} for PLS1) of responses.}

\item{ncomp}{The number of scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining the statistical weights to apply to the row observations.}

}

\details{

The functions center \eqn{X} and \eqn{Y} before the analysis, and normalize the eventual statistical weights to sum to 1 (the \code{weights} input is automatically tranformed to \code{weights/sum(weights)}). If all the weights are set to be equal, \code{pls.kernelw} returns the same results as \code{pls.kernel}.

}

\value{
A list of outputs, such as:

\item{T}{The X-score matrix \eqn{Tr}  (\eqn{n x ncomp}).}

\item{P}{The X-loadings matrix (\eqn{p x ncomp}).}

\item{W}{The X-loading weights matrix (\eqn{p x ncomp}).}

\item{C}{The Y-loading weights matrix (C = B', where B is the scores regression coefficients matrix).}

\item{R}{The PLS projection matrix (\eqn{p x ncomp}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

\item{ymeans}{The centering vector of \eqn{Y} (length \eqn{q}).}

}

\references{

Andersson, M., 2009. A comparison of nine PLS1 algorithms. Journal of Chemometrics 23, 518-529.

Dayal, B.S., MacGregor, J.F., 1997. Improved PLS algorithms. Journal of Chemometrics 11, 73-85.

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighted partial least squares and statistical wavelength selection. Int. J. Pharm., 421, 269-274.

Lesnoff, M., Metz, M., Roger, J.M.. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR Data. Submitted to Journal of Chemometrics.

Schaal, S., Atkeson, C., Vijayamakumar, S. 2002. Scalable techniques from nonparametric statistics for the real time robot learning. Applied Intell., 17, 49-60.

Sicard, E. Sabatier, R., 2006. Theoretical framework for local PLS1 regression and application to a rainfall data set. Comput. Stat. Data Anal., 51, 1393-1410.

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wold, S., Sjostrom, M., Eriksson, l., 2001. PLS-regression: a basic tool for chemometrics. Chem. Int. Lab. Syst., 58, 109-130.

}

\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

pls.kernel(X, y1, ncomp = 3)

pls.kernel(X, Y, ncomp = 3)

pls.kernel(X, Y, ncomp = 3)$T
pls.kernelw(X, Y, ncomp = 3, weights = rep(1, n))$T

pls.kernelw(X, Y, ncomp = 3, weights = 1:n)$T
pls.nipalsw(X, Y, ncomp = 3, weights = 1:n)$T

}

\keyword{datagen}