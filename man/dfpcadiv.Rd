\name{dfpcadiv}
\alias{dfpcadiv}
\encoding{latin1}

\title{Degrees of freedom of PCA Models}

\description{

Monte Carlo estimation of the model complexity (\eqn{df} = number of degrees of freedom) of PCA models. See in particular Ye 1998 and Efron 2004. Other details and references are given in the help page of function (\code{\link{dfplsrcov}}.

This is a pedagogical function since the true \eqn{df} of a PCA model can be computed directly. For a PCA of dimension \eqn{a}, \eqn{df = p + a * (n - 1) + p * a - a^2} (Faber et al. 1994, Faber 2012, Josse & Husson 2012).  

}

\usage{

dfpcadiv(X, ncomp, algo = NULL, 
  ns = 50, eps = 1e-4, 
  seed = NULL, print = TRUE, ...) 

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of training observations.}

\item{ncomp}{The maximal number of PLS scores (= components = latent variables) to consider.}

\item{algo}{a PCA algorithm. Default to  \code{NULL} (see \code{\link{pca}}).}

\item{ns}{Number of observations in the data receiving perturbation. The maximum is \eqn{min(n, p)}.}

\item{eps}{For \code{dfplsrdiv}. The \eqn{epsilon} quantity used for scaling the perturbation analysis.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of outputs (see examples), such as:

\item{df}{The model complexity for the models with \eqn{a = 0, 1, ..., ncomp} components.}

}

\references{

Efron, B., 2004. The Estimation of Prediction Error. Journal of the American Statistical Association 99,
619–632. https://doi.org/10.1198/016214504000000692

Faber, N.M., Buydens, L.M.C., Kateman, G., 1994. Aspects of pseudorank estimation methods based on the eigenvalues of principal component analysis of random matrices. Chemometrics and Intelligent Laboratory Systems 25, 203–226. https://doi.org/10.1016/0169-7439(94)85043-7

Faber, N. (Klaas) M., 2008. Degrees of freedom for the residuals of a principal component analysis — A clarification. Chemometrics and Intelligent Laboratory Systems 93, 80–86. https://doi.org/10.1016/j.chemolab.2008.04.006

Josse, J., Husson, F., 2012. Selecting the number of components in principal component analysis using cross-validation approximations. Computational Statistics & Data Analysis 56, 1869–1879. https://doi.org/10.1016/j.csda.2011.11.012

Ye, J., 1998. On Measuring and Correcting the Effects of Data Mining and Model Selection. Journal of
the American Statistical Association 93, 120–131. https://doi.org/10.1080/01621459.1998.10474094

}

\examples{

data(datoctane)

z <- datoctane$X
u <- which(!is.na(rowSums(z)))    ## Removing rows with NAs
X <- z[u, -4]
dim(X)

ncomp <- 20
fm <- dfpcadiv(X, ncomp, ns = 30)
zncomp <- 0:ncomp
plot(zncomp, fm$df, xlab = "Nb. Components", ylab = "Degrees of freedom")
## True df
lines(zncomp, fm$dftrue, col = "grey")
fm

}

\keyword{datagen}