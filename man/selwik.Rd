\name{selwik}
\alias{selwik}
\encoding{latin1}

\title{Heuristic selection of the dimension of PLSR models with a permutation test on scores}

\description{

The function helps selecting the dimension (i.e. nb. components) of PLSR models.

The method was proposed by Wiklund et al. 2007 and Faber et al. 2007. For a given PLS score \eqn{t}, the principle is to compare the observed covariance \eqn{Cov(Y, t)} (where \eqn{Y} is the response) to the distribution \eqn{H0} of simulated \eqn{Cov(Y, t)} computed on randomly permuted data (in which the relation between \eqn{Y} and \eqn{X} is assumed being removed). A significant observed covariance compared to distribution \eqn{H0} is expected indicating a meaningful dimension.

The method can be time-consuming, especially for large datasets, since permutations are conditional to each component taken successively (successive one-dimension PLSR). A one-dimension PLSR is firstly implemented, data \eqn{Y} are randomly permuted (referred to as "\eqn{Y}-scambling"), and distribution \eqn{H0} is computed. Then, information contained in the first dimension is removed from the data by deflation, and a the next dimension is studied by a new one-dimension PLSR, and so on.

Wiklund et al. 2007 and Faber et al. 2007 presented the method for PLSR1 models only (univariate \eqn{Y}). The function extends the method to PLSR2 (multivariate \eqn{Y}). 

The function returns the p-value of the on-side test, i.e. the proportion of distribution \eqn{H0} higher than the observed covariance. 

}

\usage{

selwik(
    X, Y, ncomp, 
    algo = NULL, weights = NULL,
    nperm = 50, seed = NULL, 
    print = TRUE, 
    ...
    )

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{A \eqn{n x q} matrix or data frame, or vector of length \eqn{n} for PLS1, of responses.}

\item{ncomp}{The maximal number of scores (i.e. components = latent variables) to be calculated.}

\item{algo}{A function implementing a PLS. Default to  \code{NULL} (\code{\link{pls_kernel}} is used). }

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{nperm}{Number of random permutations.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}


\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list with outputs, see the examples.

}

\references{

Faber, N.M., Rajko, R., 2007. How to avoid over-fitting in multivariate calibration—The conventional validation approach and an alternative. Analytica Chimica Acta, Papers presented at the 10th International Conference on Chemometrics in Analytical Chemistry 595, 98-106. https://doi.org/10.1016/j.aca.2007.05.030

Wiklund, S., Nilsson, D., Eriksson, L., Sjöström, M., Wold, S., Faber, K., 2007. A randomization test for PLS component selection. Journal of Chemometrics 21, 427–439. https://doi.org/10.1002/cem.1086

}

\examples{

data(datcass)
Xr <- datcass$Xr
yr <- datcass$yr

z <- selwik(Xr, yr, ncomp = 20, nperm = 30)
names(z)
plot(z$ncomp, z$pval,
     type = "b", pch = 16, col = "#045a8d",
     xlab = "Nb components", ylab = "p-value",
     main = "Wiklund et al. test")
alpha <- .10
abline(h = alpha, col = "grey")
u <- which(z$pval >= alpha)
opt <- min(u) - 1
opt

}

\keyword{datagen}