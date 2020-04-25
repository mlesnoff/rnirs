\name{pca.svd}
\alias{pca.svd}
\alias{pca.eigen}
\alias{pca.nipals}
\alias{pca.nipalsna}
\alias{pca.sph}
\alias{pca.cr}
\alias{pca.rob}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms for (centered) PCA of a matrix \eqn{X}.

\bold{Usual PCA}

- \code{pca.svd}: SVD decomposition, using function \code{\link{svd}}. 

- \code{pca.eigen}: eigen decomposition, using function \code{\link{eigen}}. 

- \code{pca.nipals}: NIPALS. 

- \code{pca.nipalsna}: NIPALS allowing missing data in \eqn{X} (\code{NA} are not allowed in the other algorithms). The core of \code{pca.nipals} uses the code of function \code{nipals} of package \code{nipals} v.0.7 (K. Wright, 2020) available on CRAN.

\bold{Robust PCA}

- \code{pca.sph}: Spherical PCA (see Locantore et al. 1990, Daszykowski et al. 2007). The spatial median (used for centering) is calculated by function \code{.xmedspa} in file \code{zfunctions.R} that uses the fast code of rrcov v.1.4-3 available on R CRAN (V. Todorov, 2016).

- \code{pca.cr}: Croux & Ruiz-Gazen (CR) PCA algorithm using projection pursuit (PP) method (Croux & Ruiz-Gazen 2005). In the CR algorithm, the 1st loading vector is choosen within the \eqn{n} data observations (rows of \eqn{X}). The next loadings are choosen in the same way after subsequent deflation of matrix \eqn{X}. One of the possible improvements  of the algorithm is to simulate (randomly) additionnal candidate PP directions. In function \code{pca.cr}, this is done when argument \code{nrep > 0}. In such a case, the function simulates \eqn{nrep * n} additional PP directions to the \eqn{n} initial ones, by using the method of Hubert et al. (2005): random couples of observations are sampled and, for each couple, the direction passes through the two data points of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

- \code{pca.rob}: Robust PCA using an algorithm built in the same philosophy (but with differences) as the ROBPCA method (Hubert et al. 2005, 2009). The algorithm is in the three following steps. 

Step 1 intends to detect multivariate outliers (potentially with bad leverages) that will receive a weight 0 in the second step. In function \code{pca.rob}, two possibles detection methods can be used: (a) the Stahel-Donoho outlyingness (Maronna and Yohai, 1995) using PP such as in ROBPCA (see functions \code{.simpp.hub} and \code{.stahel} in file \code{zfunctions.R}), (b) the Mahalanobis distance to center (score distance SD) after a spherical PCA. Depending arguments \code{cri} and \code{alpha} in the function, the outliers are either the \eqn{(1 - alpha)} proportion of the observations with the highest outlyingness (or SD) (by default \eqn{alpha = .75} in Hubert et al. 2005, 2009), or the observations having an outlyingness (or SD robustly centered and scaled by median and MAD) > \eqn{cri}, where \eqn{cri} is a cutoff value. 

Step 2 starts by calculating a (first) robust score space, by doing a weighted PCA using weights \eqn{w1}. Then scores and orthogonal distances (SD and OD) of the observations are then calculated in this score space, and respective weights (\eqn{w2} for SD and \eqn{w3} for OD) are calculated by a decreasing function (see \code{\link{wdist}}).

Step 3 consists in the weighted PCA of \eqn{X} with weights \eqn{w = w2 * w3}, giving the second and final robust score space.

Instead of step2-3, ROBPCA (Hubert et al. 2005) uses successive spectral decompositions of minimum covariance determinant (MCD) estimations of robust covariance matrices, and parametric distribution hypotheses for defining SD and OD cutoffs.

\bold{Argument} \code{weights}

Some of the functions above can give a priori weights to the observations (rows of \eqn{X}), with argument \code{weights}. This modifies the importance given to each of the \eqn{n} observations in the calculations of the scores and loadings. For instance, function \code{pca.svd} implements a SVD of \eqn{D^(1/2) X}, where D = diag(weights) and X has been centered with metric \eqn{D}. Function \code{pca.eignenw} implements an eigen decomposition of \eqn{X' D X}.

}

\usage{

pca.svd(X, ncomp, weights = NULL)

pca.eigen(X, ncomp, weights = NULL)

pca.nipals(X, ncomp, weights = NULL,
  tol = .Machine$double.eps^0.5, maxit = 100)

pca.nipalsna(X, ncomp, gramschmidt = TRUE,
  tol = .Machine$double.eps^0.5, maxit = 100)
  
pca.sph(X, ncomp, weights = NULL)

pca.cr(X, ncomp, obj = c("mad", "sd"), nrep = 0)

pca.rob(X, ncomp, w1 = c("pp", "sph"), 
  nrep = 1, ncomp.sph = 10, cri = 2.5, alpha = NULL, h = 2)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The number of PCA scores (i.e. components) to be calculated.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\bold{Specific arguments of} \code{pca.nipalsna}

\item{gramschmidt}{Logical. If TRUE (default), when there are missing data, a Gram-Schmidt orthogonalization is implemented at each iteration of the NIPALS algorithm. This slightly corrects the scores to insure that they are orthogonal. See vignettes in package \code{nipals}.}

\item{tol}{Tolerance for testing convergence of the NIPALS iterations for each principal component.}

\item{maxit}{Maximum number of NIPALS iterations for each principal component.}

\bold{Specific arguments of} \code{pca.cr}

\item{obj}{The objective scale function to maximize with the PP scores. Possible values are "mad" (default; = MAD) or "sd" (standard deviation, corresponding to the usual PCA objective). }

\item{nrep}{In the original CR algorithm, the starting considered PP directions are the \eqn{n} observations (rows of \eqn{X}). If \code{nrep > 0}, a number of \eqn{nrep * n}  additionnal PP directions are randomly simulated (see section Description). Default to \code{nrep = 0} (no additionnal simulation). This the same argument for step 1 of function \code{pca.rob} below, but default is set to \code{nrep = 1}

}

\bold{Specific arguments of} \code{pca.rob}

\item{w1}{Method used in step 1 for computing weights \eqn{w1} (see section Description). Possible values are "pp" (PP, default) or "sph" (spherical PCA).}

\item{ncomp.sph}{Used in step 1 if \code{w1 = "sph"}. The number of components considered in the spherical PCA for defining the score space (see section Description). Default to \code{min(10, ncol(X))}.}

\item{cri}{Used in step 1 if \code{alpha = NULL}. The cutoff value for defining outliers (see section Description). Default to \code{cri = 2.5}.}

\item{alpha}{Used in step 1 if not \code{NULL}. The outliers are the \eqn{(1 - alpha)} proportion of the observations with the highest outlyingness (or SD). Default to \code{NULL} (argument \code{cri} is used instead).}

\item{h}{Used in step 2. A scalar defining the shape of the decreasing weight function. See function \code{\link{wdist}}.}

}

\value{

A list of outputs, such as:

\item{T}{The score matrix (\eqn{n x ncomp}).}

\item{P}{The loadings matrix (\eqn{p x ncomp}).}

\item{R}{The projection matrix (= \eqn{P} ; \eqn{p x ncomp}).}

\item{sv}{The singular values (vector of length \eqn{ncomp}).}

\item{xss}{The eigenvalues (\code{sv^2}, vector of length \eqn{ncomp}).}

\item{xmeans}{The centering vector of \eqn{X} (length \eqn{p}).}

}

\references{

Daszykowski, M., Kaczmarek, K., Vander Heyden, Y., Walczak, B., 2007. Robust statistics in data analysis - A review. Chemometrics and Intelligent Laboratory Systems 85, 203-219. https://doi.org/10.1016/j.chemolab.2006.06.016

Gabriel, R. K., 2002. Le biplot - Outil d'exploration de données multidimensionnelles. Journal de la Société Française de la Statistique, 143, 5-55.

Hubert, M., Rousseeuw, P.J., Vanden Branden, K., 2005. ROBPCA: A New Approach to Robust Principal Component Analysis. Technometrics 47, 64-79. https://doi.org/10.1198/004017004000000563

Hubert, M., Rousseeuw, P., Verdonck, T., 2009. Robust PCA for skewed data and its outlier map. Computational Statistics & Data Analysis 53, 2264-2274. https://doi.org/10.1016/j.csda.2008.05.027

N. Locantore, J.S. Marron, D.G. Simpson, N. Tripoli, J.T. Zhang, K.L. Cohen, Robust principal component analysis for functional data, Test 8 (1999) 1–7

Maronna, R.A., Yohai, V.J., 1995. The Behavior of the Stahel-Donoho Robust Multivariate Estimator. Journal of the American Statistical Association 90, 330-341. https://doi.org/10.1080/01621459.1995.10476517

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wright, K., 2018. Package nipals: Principal Components Analysis using NIPALS with Gram-Schmidt Orthogonalization. https://cran.r-project.org/

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