\name{pls.kernel}
\alias{pls.kernel}
\alias{pls.nipals}
\alias{pls.rannar}
\encoding{latin1}

\title{PLS algorithms}

\description{

PLS1 and PLS2 algorithms.

\code{pls.nipals} implements the NIPALS algorithm (e.g. Tenenhaus 1998, Wold 2002). However, for PLS2, the usual NIPALS iterative part is replaced by the direct calculation of the weights vector \eqn{w} by SVD decomposition of matrix \eqn{X'Y} (Hoskuldsson 1988 p.213).  

\code{pls.kernel} implements the "improved kernel algorithm #1" proposed by Dayal and MacGregor (1997). This algorithm is stable and fast (Andersson 2009), and returns the same results as NIPALS. 

\code{pls.rannar} implements a kernel algorithm proposed by Rannar et al. (1994) for "wide" matrices, i.e.  with low number of rows and very large number of columns (p >> n; e.g. p = 20000). In such a situation, this algorithm is faster than the others (but becomes much slower in other situations). If the algorithm convergences, it returns the same results as NIPALS (Note: discrepancies can be observed if too many PLS components are requested compared to the low number of observations).

All algorithms center matrix \eqn{X}. Missing values are not allowed.

The functions can give a priori weights to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores, loadings and predictions.

Data are internally centered before the analyses, but they are not column-wise scaled. If needed, the user has to do the scaling before using the functions (there is no argument \code{scale} in the functions).

}

\usage{

pls.kernel(X, Y, ncomp, weights = NULL)

pls.nipals(X, Y, ncomp, weights = NULL)

pls.rannar(X, Y, ncomp, weights = NULL)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{A \eqn{n x q} matrix or data frame (or vector of length \eqn{n} for PLS1) of responses.}

\item{ncomp}{The number of scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

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

Hoskuldsson, A., 1988. PLS regression methods. Journal of Chemometrics 2, 211-228. https://doi.org/10.1002/cem.1180020306

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighted partial least squares and statistical wavelength selection. Int. J. Pharm., 421, 269-274.

Lesnoff, M., Metz, M., Roger, J.M., 2020. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR Data. Journal of Chemometrics. e3209. https://onlinelibrary.wiley.com/doi/abs/10.1002/cem.3209

Rannar, S., Lindgren, F., Geladi, P., Wold, S., 1994. A PLS kernel algorithm for data sets with many variables and fewer objects. Part 1: Theory and algorithm. Journal of Chemometrics 8, 111-125. https://doi.org/10.1002/cem.1180080204

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

pls.kernel(X, Y, ncomp = 3, weights = 1:n)$T

pls.nipals(X, Y, ncomp = 3, weights = 1:n)$T

}

\keyword{datagen}