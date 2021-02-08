\name{selscree}
\alias{selscree}
\encoding{latin1}

\title{Scree plots for PCA or PLS}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA or PLS model by ploting the variability of the scores.

Depending on the following values of argument \code{scree}

- \code{"log"} (default): log-eigenvalues are plotted (e.g. Faber et al. 1994). Patterns in eigen-values are often more apparent in the logarithm scale. Eigenvalues associated with eigenvectors that are dominated by noise are expected decaying geometrically (Cangelosi & Goriely 2007) leading to a straight line in the log-scale.

- \code{"eig"}: raw eigenvalues are plotted. For a PCA model, eigenvalues are the output \code{eig} of the fitted model. For a PLS model, they are the variances of the scores \code{T}.    

- \code{"split"}: Observation score values (i.e. rows of \code{T}) are plotted.

}

\usage{

selscree(fm, ncomp = NULL,
    scree = c("log", "eig", "split"),
    ...)

}

\arguments{

\item{fm}{A PCA or PLS model (e.g. output of \code{\link{pca_eigen}} or \code{\link{pca}}).}

\item{ncomp}{The number of components (latent variables) to consider.}

\item{scree}{Type of plot. Possible values are "log" (Default; log-eigenvalues), "eig" (eigenvalues) or "split" (score values).} 

\item{...}{Optionnal arguments to pass through function \code{\link{plotsl}}.}

}

\value{

A list of outputs, see the examples.

}

\references{

Cangelosi, R., Goriely, A., 2007. Component retention in principal component analysis with application to cDNA microarray data. Biology Direct 2, 2. https://doi.org/10.1186/1745-6150-2-2

Faber, N.M., Buydens, L.M.C., Kateman, G., 1994. Aspects of pseudorank estimation methods based on the eigenvalues of principal component analysis of random matrices. Chemometrics and Intelligent Laboratory Systems 25, 203-226. https://doi.org/10.1016/0169-7439(94)85043-7

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
plotsp(zX)

ncomp <- 30
fm <- pca_eigen(zX, ncomp = ncomp)

selscree(fm)
selscree(fm, scree = "eig")
selscree(fm, scree = "split")
z <- selscree(fm, scree = "eig", plot = "none")
z

## Random matrix with uncorrelated structure
n <- 50 
p <- 1000
z <- matrix(rnorm(n * p), nrow = n)
fm <- pca_eigenk(z, ncomp = 40)
selscree(fm, plot = "diff")

}

\keyword{datagen}