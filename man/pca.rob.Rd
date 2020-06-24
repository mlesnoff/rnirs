\name{pca.rob}
\alias{pca.rob}
\alias{pca.cr}
\alias{pca.sph}
\encoding{latin1}

\title{Robust PCA algorithms}

\description{

Algorithms for robust (centered) PCA of a matrix \eqn{X}.

- \code{pca.cr}: 

Croux & Ruiz-Gazen (C-R) PCA algorithm using projection pursuit (PP) method (Croux & Ruiz-Gazen 2005). The observations are robustly centered, and projected to specific "PP" directions (see below) of the spaned by the variables (\eqn{X} columns). The PCA loading vectors are the directions (within these PP directions) that maximize a given "projection index", usually a robust spread measure such as MAD. The 1st loading vector is choosen within the \eqn{n} directions corresponding the observations (rows of \eqn{X}). The next loading vector is choosen in \eqn{n} directions corresponding the rows of the deflated matrix \eqn{X}). And so on.

A possible extension  of the algorithm is to randomly simulate additionnal candidate PP directions to the \eqn{n} row observations. In function \code{pca.cr}, this is done when argument \code{nsim > 0}. In such a case, the function simulates \eqn{nsim} additional PP directions to the \eqn{n} initial ones, as proposed in Hubert et al. (2005): random couples of observations are sampled in \eqn{X} and, for each couple, the direction passes through the two observations of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

- \code{pca.sph}: 

Spherical PCA (see Locantore et al. 1990, Maronna 2005, Daszykowski et al. 2007). The spatial median used for centering matrix \eqn{X} is calculated by function \code{.xmedspa} (available in file \code{zfunctions.R}). This function uses the fast code of rrcov v.1.4-3 available on R CRAN (Thanks to V. Todorov, 2016).

- \code{pca.rob}: 

Robust PCA combining outlyingness measures and weighted PCA (WPCA). Rows of matrix \eqn{X} receive weights depending on outlyingness. The algorithm is in the same spirit (but quite different) as the ROBPCA method (Hubert et al. 2005, 2009). It is built in the two following steps. Note that the second step is facultative (argument \code{step2}). 

Step 1 intends to detect and remove multivariate \eqn{X-}outliers that have potentially bad leverages. First, the Stahel-Donoho outlyingness (Maronna and Yohai, 1995) is computed for each of the rows of matrix \eqn{X} (see \code{\link{out.stah}}). Noting \eqn{alpha} a proportion (by default in the function, \eqn{alpha = .75},  as in Hubert et al. 2005, 2009), the  \eqn{(1 - alpha) * n} observations that have the highest outlyingness receive a weight \eqn{w1 = 0} (the other receive a weight \eqn{w1 = 1}). Then, a robust Euclidean outlyingness (see \code{\link{out.eucl}}) is calculated. The  \eqn{(1 - alpha) * n} observations that have the highest outlyingness receive a weight \eqn{w2 = 0} (the other receive a weight \eqn{w2 = 1}). The final weight for Step 1 is \eqn{w = w1 * w2}), which is used for a first weighted PCA.  

Step2 (facultative) intends to detect outliers in the robust score space computed in Step 1. The score and orthogonal distances, standardized by given cutoffs (see \code{\link{scordis}} and \code{\link{odis}}), of each of the \eqn{n} observations are calculated from this score space. An outlyingness is computed by \eqn{(.5 * SD.stand^2 + .5 * OD.stand^2)^.5}. The  \eqn{(1 - alpha) * n} observations that have the highest outlyingness receive a weight \eqn{w = 0} (the other receive a weight \eqn{w = 1}), and a new weighted PCA is fitted.

\bold{Argument} \code{weights}

Some of the functions above can give a priori weights to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores and loadings.

Data are internally centered before the analyses, but they are not column-wise scaled. If needed, the user has to do the scaling before using the functions (there is no argument \code{scale} in the functions).

}

\usage{

pca.cr(X, ncomp, obj = mad, nsim = 0)

pca.sph(X, ncomp, weights = NULL)

pca.rob(X, ncomp, nsim = 1500, alpha = .70, step2 = TRUE, kern = NULL, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{obj}{For \code{pca.cr}. The objective function (projection index) to maximize with the PP scores. Default to \code{obj = mad}. }

\item{nsim}{For \code{pca.cr} and \code{pca.rob}. Used for the Stahel-Donoho outlyingness calculation (See \code{\link{out.stah}}): a number of randomly simulated PP (projection pursuit) directions considered in addition to the \eqn{n} observations (rows of \eqn{X}).}

\item{weights}{For \code{pca.sph}. A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{alpha}{For \code{pca.rob}. Proportion \eqn{1 - alpha} is used for hard rejection of outliers in Step 1 (default to \code{alpha = .70}).}

\item{step2}{For \code{pca.rob}. If \code{TRUE} (default), Step 2 is implemented.}

\item{kern}{For \code{pca.rob}. Used in \code{\link{pca.eigen}}.}

\item{...}{For \code{pca.rob}. Optional arguments to pass in function \code{\link{out.pca}}.}

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

n <- 6
p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(NULL)
X

pca.svd(X, ncomp = 3)

pca.eigen(X, ncomp = 3)

pca.nipals(X, ncomp = 3)

pca.sph(X, ncomp = 3)

pca.cr(X, ncomp = 3)

pca.rob(X, ncomp = 3)

######## WITH MISSING DATA

X2 <- X
X2[3, 3] <- X2[1, 3] <- X2[1, 2] <- NA
X2

fm <- pca.nipalsna(X2, ncomp = 3)
fm

## Replacement of the missing data in X2
## by their NIPALS estimates

Xhat <- xfit(fm$T, fm$P, fm$xmeans)
Xhat
u <- which(is.na(X2))
Xfull <- replace(X2, u, Xhat[u])
Xfull

}

\keyword{datagen}