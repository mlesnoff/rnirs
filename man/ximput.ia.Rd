\name{ximputia}
\alias{ximputia}
\encoding{latin1}

\title{Missing Data Imputation using PCA and the Iterative Algorithm (IA)}

\description{

Imputation of missing data contained in a matrix \eqn{X} using PCA and the so-called "iterative algorithm" (IA).

Missing data are estimated from iterative calculations of PCA scores and loadings. Intial estimates are given to the missing data, first scores and loadings matrices (\eqn{T} and \eqn{P}) are calculated, giving new estimates (from \eqn{T P'}). The process is repeated until convergence or a maximal number of iterations (possibly equal to 1).

Depending on argument \eqn{start}, the initial estimates for the missing data \eqn{x_ij} are calculated either by NIPALS (version allowing missing data) or the means of columns \eqn{j}.

IA is described by Walczak & Massart 2001 (section 2 p.16) and Folch-Fortuny et al. 2016 (section 2.3.3). 

It can also be considered as an expectation-maximmization (EM) algorithm. The "EM-Wold" algorithm for PCA cross-vlaidation presented by Bro et al. 2008 (p. 1245) uses such an approcah, with a single iteration (and another algorithm than NIPALS).

}

\usage{

ximputia(X, ncomp, algo = NULL,
  start = c("nipals", "means"),
  tol = .Machine$double.eps^0.5, 
  maxit = 10000,
  gs = TRUE,
  print = TRUE, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame with missing data to be imputed.}

\item{ncomp}{The number of components (latent variables) of the PCA model used for imputation.}

\item{algo}{Algorithm (e.g. \code{\link{pca.eigen}}) used for fitting the PCA model. Default to \eqn{NULL} (see \code{\link{pca}}).}

\item{start}{Method used for the initialestimate. Possible values are \code{"nipals"} (default) or \code{"means"}.}

\item{tol}{Tolerance for testing convergence of the IA algorithm.}

\item{maxit}{Maximum number of iterations for the IA algorithm.}

\item{gs}{See \code{\link{pca.nipalsna}}.}

\item{print}{Logical. If code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass through function \code{algo}.}

}

\references{

Bro, R., Kjeldahl, K., Smilde, A.K., Kiers, H.A.L., 2008. Cross-validation of component models: A critical look at current methods. Anal Bioanal Chem 390, 1241-1251. https://doi.org/10.1007/s00216-007-1790-1

de La Fuente, R.L.-N. de la, García‐Muñoz, S., Biegler, L.T., 2010. An efficient nonlinear programming strategy for PCA models with incomplete data sets. Journal of Chemometrics 24, 301-311. https://doi.org/10.1002/cem.1306

Folch-Fortuny, A., Arteaga, F., Ferrer, A., 2016. Missing Data Imputation Toolbox for MATLAB. Chemometrics and Intelligent Laboratory Systems 154, 93-100. https://doi.org/10.1016/j.chemolab.2016.03.019

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

############################ NAs simulated in a row of X

zX <- X
## Row i
i <- 18
## 20pct of NAs in row i
s <- sample(1:p, size = round(p / 5))
zX[i, s] <- NA

## Nipals alone

fm <- ximputia(zX, ncomp = 5, maxit = 1)   
names(fm)
Xfit <- fm$X
plot(1:p, X[i, ], type = "l")
points(s, Xfit[i, s], col = "red")

## With iterations

fm <- ximputia(zX, ncomp = 5)              
fm$niter
fm$conv
Xfit <- fm$X
par(mfrow = c(1, 2))
plot(1:p, X[i, ], type = "l")
points(s, Xfit[i, s], col = "red")
#sum(X[i, ] - Xfit[i, ] != 0)
plot(fm$tol, type = "b")
par(mfrow = c(1, 1))

plot(Xfit[i, s], X[i, s])
abline(0, 1)

############################ NAs simulated in X

zX <- X
## 20pct of NAs in matrix X
K <- 5
s <- sort(sample(1:N, size = round(N / K)))
zX[s] <- NA
ncomp <- 5

fm <- ximputia(zX, ncomp, maxit = 1)               ## Nipals alone
#fm <- ximputia(zX, ncomp)                         ## With iterations
#fm <- ximputia(zX, ncomp, start = "means")        ## Initial = means
fm$niter
fm$conv
if(!is.na(fm$tol[1])) plot(fm$tol)
## SSR
sum((X[s] - fm$fit)^2)

}

\keyword{datagen}