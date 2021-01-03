\name{pca_rob}
\alias{pca_rob}
\alias{pca_cr}
\alias{pca_sph}
\encoding{latin1}

\title{Robust PCA algorithms}

\description{

Algorithms implementing a robust PCA of a matrix \eqn{X}. 

- \code{pca_cr}: 

Croux & Ruiz-Gazen (C-R) PCA algorithm using projection pursuit (PP) method (Croux & Ruiz-Gazen 2005). The observations are robustly centered, and projected to specific "PP" directions (see below) of the spaned by the variables (\eqn{X} columns). The PCA loading vectors are the directions (within these PP directions) that maximize a given "projection index", usually a robust spread measure such as MAD. The 1st loading vector is choosen within the \eqn{n} directions corresponding the observations (rows of \eqn{X}). The next loading vector is choosen in \eqn{n} directions corresponding the rows of the deflated matrix \eqn{X}). And so on.

A possible extension  of the algorithm is to randomly simulate additionnal candidate PP directions to the \eqn{n} row observations. In function \code{pca_cr}, this is done when argument \code{nsim > 0}. In such a case, the function simulates \eqn{nsim} additional PP directions to the \eqn{n} initial ones, as proposed in Hubert et al. (2005): random couples of observations are sampled in \eqn{X} and, for each couple, the direction passes through the two observations of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

- \code{pca_sph}: 

Spherical PCA (see Locantore et al. 1990, Maronna 2005, Daszykowski et al. 2007). The spatial median used for centering matrix \eqn{X} is calculated by function \code{.xmedspa} (available in file \code{zfunctions.R}). This function uses the fast code of rrcov v.1.4-3 available on R CRAN (Thanks to V. Todorov, 2016).

- \code{pca_rob}: 

Robust PCA combining outlyingness measures and weighted PCA (WPCA). Rows of matrix \eqn{X} receive weights depending on outlyingness. The algorithm is in the same spirit (but quite different) as the ROBPCA method (Hubert et al. 2005, 2009). It is built in the two following steps. Note that the second step is facultative (argument \code{step2}). 

Step 1 intends to detect and remove multivariate \eqn{X-}outliers that have potentially bad leverages. First, the Stahel-Donoho outlyingness (Maronna and Yohai, 1995) is computed for each of the rows of matrix \eqn{X} (see \code{\link{outstah}}). Noting \eqn{1 - alpha} a proportion (by default in the function, \eqn{alpha = .30},  as in Hubert et al. 2005, 2009), the  \eqn{alpha * n} observations that have the highest outlyingness receive a weight \eqn{w1 = 0} (the other receive a weight \eqn{w1 = 1}). Then, a robust Euclidean outlyingness (see \code{\link{outeucl}}) is calculated. The  \eqn{(alpha * n} observations that have the highest outlyingness receive a weight \eqn{w2 = 0} (the other receive a weight \eqn{w2 = 1}). The final weight for Step 1 is \eqn{w = w1 * w2}), which is used for a first weighted PCA.  

Step2 (facultative) intends to detect outliers in the robust score space computed in Step 1. The score and orthogonal distances, standardized by given cutoffs (see \code{\link{scordis}} and \code{\link{odis}}), of each of the \eqn{n} observations are calculated from this score space. An outlyingness is computed by \eqn{(.5 * SD.stand^2 + .5 * OD.stand^2)^.5}. The  \eqn{alpha * n} observations that have the highest outlyingness receive a weight \eqn{w = 0} (the other receive a weight \eqn{w = 1}), and a new weighted PCA is fitted.

In all functions, matrix \eqn{X} is centered before the analyses, but \eqn{X} is not column-wise scaled (there is no argument \code{scale} available). If a scaling is needed, the user has to scale \eqn{X} before using the functions. 

}

\usage{

pca_cr(X, ncomp, obj = mad, nsim = 0)

pca_sph(X, ncomp, weights = NULL)

pca_rob(X, ncomp, nsim = 1500, alpha = .30, step2 = TRUE, 
                    weights = NULL, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The maximal number of PCA scores (= components = latent variables) to be calculated.}

\item{obj}{For \code{pca_cr}. The objective function (projection index) to maximize with the PP scores. Default to \code{obj = mad}. }

\item{nsim}{For \code{pca_cr} and \code{pca_rob}. Used for the Stahel-Donoho outlyingness calculation (See \code{\link{outstah}}): a number of randomly simulated PP (projection pursuit) directions considered in addition to the \eqn{n} observations (rows of \eqn{X}).}

\item{weights}{For \code{pca_sph} and \code{pca_rob}. A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{alpha}{For \code{pca_rob}. Proportion \eqn{alpha} of the data used as hard rejection (outliers) in Step 1 (default to \code{alpha = .30}, i.e. 30pct are rejected).}

\item{step2}{For \code{pca_rob}. If \code{TRUE} (default), Step 2 is implemented.}

\item{...}{For \code{pca_rob}. Optional arguments to pass in function \code{\link{outsdod}}.}

}

\value{

A list of outputs, such as:

\item{T}{The score matrix (\eqn{n x ncomp}).}

\item{P}{The loadings matrix (\eqn{p x ncomp}).}

\item{R}{The projection matrix (= \eqn{P} ; \eqn{p x ncomp}).}

\item{sv}{The singular values (vector of length \eqn{ncomp}).}

\item{eigs}{The eigenvalues (\code{= sv^2}; vector of length \eqn{ncomp}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

}

\references{

Daszykowski, M., Kaczmarek, K., Vander Heyden, Y., Walczak, B., 2007. Robust statistics in data analysis - A review. Chemometrics and Intelligent Laboratory Systems 85, 203-219. https://doi.org/10.1016/j.chemolab.2006.06.016

Hubert, M., Rousseeuw, P.J., Vanden Branden, K., 2005. ROBPCA: A New Approach to Robust Principal Component Analysis. Technometrics 47, 64-79. https://doi.org/10.1198/004017004000000563

Hubert, M., Rousseeuw, P., Verdonck, T., 2009. Robust PCA for skewed data and its outlier map. Computational Statistics & Data Analysis 53, 2264-2274. https://doi.org/10.1016/j.csda.2008.05.027

N. Locantore, J.S. Marron, D.G. Simpson, N. Tripoli, J.T. Zhang, K.L. Cohen, Robust principal component analysis for functional data, Test 8 (1999) 1–7

Maronna, R.A., Yohai, V.J., 1995. The Behavior of the Stahel-Donoho Robust Multivariate Estimator. Journal of the American Statistical Association 90, 330-341. https://doi.org/10.1080/01621459.1995.10476517

Maronna, R., 2005. Principal Components and Orthogonal Regression Based on Robust Scales, Technometrics, 47:3, 264-273, DOI: 10.1198/004017005000000166

}

\examples{

data(datoctane)

## 6 samples (25, 26, and 36–39) are outliers
## (contain added alcohol)
## See Hubert et al. 2005

X <- datoctane$X
plotsp(X)

ncomp <- 2
fm <- pca_cr(X, ncomp = ncomp)
#fm <- pca_sph(X, ncomp = ncomp)
#fm <- pca_rob(X, ncomp = ncomp)
#fm <- pca_eigen(X, ncomp = ncomp)
z <- scordis(fm)
sd <- z$dr$d
cutsd <- z$cutoff
z <- odis(fm, X)
od <- z$dr$d
cutod <- z$cutoff
plot(sd, od, pch = 16,
  xlab = "SD", ylab = "OD")
abline(h = cutod, v = cutsd, lty = 2)
u <- c(25, 26, 36:39)
points(sd[u], od[u], col = "green", cex = 1.5)

}

\keyword{datagen}