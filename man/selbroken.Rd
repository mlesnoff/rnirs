\name{selbroken}
\alias{selbroken}
\encoding{latin1}

\title{Heuristic selection of the dimension of a PCA model with the broken-stick method}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA model using the broken-stick method proposed by Frontier 1976. 

The input matrix \eqn{X} is centered and scaled internally to the function. The eigenvalues are compared to the means of the length of ordered random sections of a stick of size = 1 (broken-stick distribution).     

}

\usage{

selbroken(X, ncomp, algo = NULL,
    plot = TRUE,
    xlab = "Nb. components", ylab = NULL,
    ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The maximal number of PCA scores (= components = latent variables) to be calculated.}

\item{algo}{A function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used.}

\item{plot}{Logical. If \code{TRUE} (default), the results are plotted.}

\item{xlab}{Label for the x-axis of the plot.}

\item{ylab}{Label for the y-axis of the plot.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of several items, see the examples. Output \code{opt} is the selected number of components.

}

\references{

Bro, R., Smilde, A.K., 2014. Principal component analysis. Anal. Methods 6, 2812-2831. https://doi.org/10.1039/C3AY41907J

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
selbroken(zX, ncomp = ncomp)

}

\keyword{datagen}