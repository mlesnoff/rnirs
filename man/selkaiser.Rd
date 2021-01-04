\name{selkaiser}
\alias{selkaiser}
\encoding{latin1}

\title{Heuristic selection of the dimension of a PCA model with the Guttman-Kaiser rule}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA model using the method firstly proposed by Guttman 1954 and Kaiser 1960. 

The input matrix \eqn{X} is centered internally to the function. The eigenvalues are compared to the mean eigenvalue (\eqn{= sum(eigs) / p} for a matrix with \eqn{p} columns). If \eqn{X} is scaled and using the denominator \eqn{n} (not \eqn{n - 1}) for the variance and covariance calculations, \eqn{sum(eigs) / p = 1}.

If argument \code{ci = TRUE}, the eigenvalues are replaced by the lower limit of their 95pct confidence interval, calculated by back-transformation after assuming that \eqn{var(log(eig) = 2 / n} where \eqn{n} is the number of observations (see Karlis et al. 2003 p.647).   

}

\usage{

selkaiser(X, ncomp, algo = NULL,
    ci = FALSE, alpha = 1,
    plot = TRUE,
    xlab = "Nb. components", ylab = NULL,
    ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The maximal number of PCA scores (= components = latent variables) to be calculated.}

\item{algo}{A function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used.}

\item{ci}{Logical. If \code{TRUE}, the eigenvalues are replaced by an estimate of the lower limit of their 95pct confidence interval. Default to \code{FALSE}.}

\item{alpha}{A scalar multiplying the mean eigenvalue used for comparisons. Default to \code{alpha = 1}.}

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

Cangelosi, R., Goriely, A., 2007. Component retention in principal component analysis with application to cDNA microarray data. Biology Direct 2, 2. https://doi.org/10.1186/1745-6150-2-2

Guttman, L., 1954. Some necessary conditions for common-factor analysis. Psychometrika 19, 149–161. https://doi.org/10.1007/BF02289162

Karlis, D., Saporta, G., Spinakis, A., 2003. A Simple Rule for the Selection of Principal Components. Communications in Statistics - Theory and Methods 32, 643–666. https://doi.org/10.1081/STA-120018556

Kaiser, H.F., 1960. The application of electronic computers to factor analysis. Educational and Psychological Measurement 20, 141–151.

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
n <- nrow(zX)
plotsp(zX)

ncomp <- 30
selkaiser(zX, ncomp = ncomp)
xvar <- apply(zX, 2, var) * (n - 1) / n
selkaiser(scale(zX, scale = xvar), ncomp = ncomp)

}

\keyword{datagen}