\name{selhorn}
\alias{selhorn}
\encoding{latin1}

\title{Heuristic selection of the dimension of a PCA model with the Horn method}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA model using the method proposed by Horn 1965. 

The input matrix \eqn{X} is centered and scaled internally to the function. The eigenvalues are compared to the mean eigenvalues of a given number of random matrices of same size as \eqn{X} and with uncorrelated structure. The random matrices are built from the standart normal distribution (Dinno 2009).     

}

\usage{

selhorn(X, ncomp, algo = NULL,
    nrep = 10,
    plot = TRUE,
    xlab = "Nb. components", ylab = NULL,
    print = TRUE,
    ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The maximal number of PCA scores (= components = latent variables) to be calculated.}

\item{algo}{A function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used.}

\item{nrep}{Number of random matrices built.}

\item{plot}{Logical. If \code{TRUE} (default), the results are plotted.}

\item{xlab}{Label for the x-axis of the plot.}

\item{ylab}{Label for the y-axis of the plot.}

\item{print}{Logical. If \code{TRUE} (default), fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of several items, see the examples. Output \code{opt} is the selected number of components.

}

\references{

Dinno, A., 2009. Exploring the Sensitivity of Horn’s Parallel Analysis to the Distributional Form of Random Data. Multivariate Behavioral Research 44, 362-388. https://doi.org/10.1080/00273170902938969

Horn, J.L., 1965. A rationale and test for the number of factors in factor analysis. Psychometrika 30, 179-185. https://doi.org/10.1007/BF02289447

Jackson, D.A., 1993. Stopping Rules in Principal Components Analysis: A Comparison of Heuristical and Statistical Approaches. Ecology 74, 2204-2214. https://doi.org/10.2307/1939574

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
plotsp(zX)

ncomp <- 30
selhorn(zX, ncomp = ncomp)

}

\keyword{datagen}