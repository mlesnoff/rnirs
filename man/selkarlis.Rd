\name{selkarlis}
\alias{selkarlis}
\encoding{latin1}

\title{Heuristic selection of the number of components in a PCA with the Karlis et al. method}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA model using the method proposed by Karlis et al. 2003. 

The method is a modified version of the Guttman-Kaiser rule (see function \code{\link{selkaseir}}). The input matrix \eqn{X} is centered and scaled internally to the function. The eigenvalues are compared to the mean eigenvalue (= 1) plus an approximate standard error that accounts for estimation uncertainty (see Karlis et al. 2003).   

}

\usage{

selkarlis(X, ncomp, algo = NULL,
    plot = TRUE,
    xlab = "Nb. components", ylab = NULL,
    print = TRUE,
    ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{ncomp}{The maximal number of PCA scores (= components = latent variables) to be calculated.}

\item{algo}{A function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used.}

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

Karlis, D., Saporta, G., Spinakis, A., 2003. A Simple Rule for the Selection of Principal Components. Communications in Statistics - Theory and Methods 32, 643–666. https://doi.org/10.1081/STA-120018556

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
plotsp(zX)

ncomp <- 30
selkarlis(zX, ncomp = ncomp)

}

\keyword{datagen}