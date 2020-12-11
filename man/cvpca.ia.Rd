\name{cvpca.ia}
\alias{cvpca.ia}
\alias{cvpca.tri}
\alias{cvpca.ckf}
\encoding{latin1}

\title{Cross-validation of a PCA model by Missing Data Imputation}

\description{

The general principle is to replace segments (i.e sub-samples \eqn{{x_ij}}) of matrix \eqn{X} by missing values, and then to estimate these segments by algorithms of missing data imputation (MDI). PRESS are then calculated on these segments.

- \code{cvpca.ia}: For a given segment, the location of the missing data is simulated randomly in matrix \eqn{X}. Missing data are then estimated by the PCA "iterative algorithm" (IA), see function \code{\link{ximput.ia}}. If NIPALS is used in the first iteration, the algorithm can be slow for large matrix  \eqn{X}. It is also recommanded to check the convergence of the MDI estimations in the outputs. 

- \code{cvpca.tri}: This is the "efficient ekf-TRI" algorithm proposed by Camacho & Ferrer 2012 (p.370), referred to as "Algorithm2" in Saccenti & Camacho 2015a. It returns equal results (but with faster computation time) to algorithm "ekf-TRI". This algorithm is for instance presented by Camacho & Ferrer 2012 (see "Algorithm2"). A given segment is defined by a selection of rows in \eqn{X}. Then, for each row of the segment, sets of colums are successively considered as missing, and the corresponding missing data \eqn{x_ij} are estimated with the trimmed score method (TRI) (e.g. Nelson et al. 1996, Arteag & Ferrer 2002). In the present version of the function, each column is removed one by one, which is a column leave-one out (LOO).  
  
- \code{cvpca.ckf}: This is the "ckf-TRI" algorithm proposed by Saccenti & Camacho 2015 ("Algorithm3" p.469). It can be considered as a simplification of the "efficient ekf-TRI". The TRI is still used for missing data imputation but data are only removed column-wise. Only one PCA model is fitted (there are no selections of rows), and the algorithm is therfore very fast compared to others.

}

\usage{

cvpca.ia(X, ncomp, algo = NULL,
  segm,
  start = "nipals",
  tol = .Machine$double.eps^0.5, 
  maxit = 10000, 
  print = TRUE, ...)

cvpca.tri(X, ncomp, algo = NULL, 
  segm, 
  print = TRUE, ...)

cvpca.ckf(X, ncomp, algo = NULL, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame.}

\item{ncomp}{The number of PCA components (latent variables).}

\item{segm}{A list of the test segments. Typically, output of function \code{\link{segmkf}} or \code{\link{segmts}}.} 

\item{algo}{Algorithm (e.g. \code{\link{pca.eigen}}) used for fitting the PCA model. Default to \eqn{NULL} (see \code{\link{pca}}).}

\item{start}{Method used for the initial estimate in the IA algorithm. Possible values are \code{"nipals"} (default) or \code{"means"}.}

\item{tol}{Tolerance for testing convergence of the IA algorithm.}

\item{maxit}{Maximum number of iterations for the IA algorithm.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass through function \code{algo}.}

}

\references{

Arteaga, F., Ferrer, A., 2002. Dealing with missing data in MSPC: several methods, different interpretations, some examples. Journal of Chemometrics 16, 408–418. https://doi.org/10.1002/cem.750

Arteaga, F., Ferrer, A., 2005. Framework for regression-based missing data imputation methods in on-line MSPC. Journal of Chemometrics 19, 439–447. https://doi.org/10.1002/cem.946

Bro, R., Kjeldahl, K., Smilde, A.K., Kiers, H.A.L., 2008. Cross-validation of component models: A critical look at current methods. Anal Bioanal Chem 390, 1241-1251. https://doi.org/10.1007/s00216-007-1790-1

Camacho, J., Ferrer, A., 2012. Cross-validation in PCA models with the element-wise k-fold (ekf) algorithm: theoretical aspects. Journal of Chemometrics 26, 361–373. https://doi.org/10.1002/cem.2440

Camacho, J., Ferrer, A., 2014. Cross-validation in PCA models with the element-wise k-fold (ekf) algorithm: Practical aspects. Chemometrics and Intelligent Laboratory Systems 131, 37–50. https://doi.org/10.1016/j.chemolab.2013.12.003

de La Fuente, R.L.-N., García‐Muñoz, S., Biegler, L.T., 2010. An efficient nonlinear programming strategy for PCA models with incomplete data sets. Journal of Chemometrics 24, 301-311. https://doi.org/10.1002/cem.1306

Folch-Fortuny, A., Arteaga, F., Ferrer, A., 2015. PCA model building with missing data: New proposals and a comparative study. Chemometrics and Intelligent Laboratory Systems 146, 77–88. https://doi.org/10.1016/j.chemolab.2015.05.006

Folch-Fortuny, A., Arteaga, F., Ferrer, A., 2016. Missing Data Imputation Toolbox for MATLAB. Chemometrics and Intelligent Laboratory Systems 154, 93-100. https://doi.org/10.1016/j.chemolab.2016.03.019

Nelson, P.R.C., Taylor, P.A., MacGregor, J.F., 1996. Missing data methods in PCA and PLS: Score calculations with incomplete observations. Chemometrics and Intelligent Laboratory Systems 35, 45-65. https://doi.org/10.1016/S0169-7439(96)00007-X

Saccenti, E., Camacho, J., 2015a. On the use of the observation-wise k-fold operation in PCA cross-validation. Journal of Chemometrics 29, 467–478. https://doi.org/10.1002/cem.2726

Saccenti, E., Camacho, J., 2015b. Determining the number of components in principal components analysis: A comparison of statistical, crossvalidation and approximated methods. Chemometrics and Intelligent Laboratory Systems 149, 99–116. https://doi.org/10.1016/j.chemolab.2015.10.006

Walczak, B., Massart, D.L., 2001. Dealing with missing data: Part I. Chemometrics and Intelligent Laboratory Systems 58, 15-27. https://doi.org/10.1016/S0169-7439(01)00131-9

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
n <- nrow(zX)
p <- ncol(zX)
N <- n * p
plotsp(zX)

##### IA

K <- 5
segm <- segmkf(n = N, K = K, nrep = 1, seed = 1)
#segm <- segmts(n = N, m = N / K, nrep = K)
ncomp <- 10
fm <- cvpca.ia(zX, ncomp, segm = segm, maxit = 1)     ## NIPALS alone
#fm <- cvpca.ia(zX, ncomp, segm = segm)               ## With iterations
names(fm)
head(fm$res.summ)
head(fm$res)
fm$niter
fm$conv
fm$opt
z <- fm$res.summ
u <- selwold(z$msep[-1], start = 1, alpha = 0, main = "MSEP_CV")

##### TRI

K <- 5
ncomp <- 20
segm <- segmkf(n = n, K = K, nrep = 1, seed = 1)
fm <- cvpca.tri(zX, ncomp, segm = segm)             
names(fm)
fm$opt
z <- fm$res.summ
u <- selwold(z$msep[-1], start = 1, alpha = 0, main = "MSEP_CV")

##### TRI-CKF

ncomp <- 20
fm <- cvpca.ckf(zX, ncomp)
names(fm)
fm$opt
z <- fm$res
u <- selwold(z$msep[-1], start = 1, alpha = 0, main = "MSEP_CV")

}

\keyword{datagen}