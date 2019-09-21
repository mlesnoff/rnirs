\name{xfit}
\alias{xfit}
\encoding{latin1}

\title{Matrix Approximation from multivariate scores and loadings}

\description{
Calculation of the approximate of a matrix \eqn{X} from multivariate (e.g. PCA or PLS) scores and loadings.
}

\usage{xfit(T, P, xmeans = NULL)}

\arguments{
\item{T}{A \eqn{n x a} scores matrix, or data.frame, calculated from a \eqn{n x p} matrix \eqn{X}.}
\item{P}{A \eqn{p x a} loadings matrix, or data.frame, that will be applied to \eqn{T}.}
\item{xmeans}{An eventual centering vector (length \eqn{p}) used for \eqn{X} before calculating \eqn{T}.}
}

\value{Approximate of matrix \eqn{X}.}

\examples{

data(datcass)

Xr <- datcass$Xr
Xu <- datcass$Xu
Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
dim(Xr)
dim(Xu)

ncomp <- 2
#ncomp <- 10
fm <- pca(Xr, Xu, ncomp = ncomp)

Xr_approx <- xfit(fm$Tr, fm$P, fm$xmeans)
dim(Xr_approx)
s <- sample(1:nrow(Xr), size = 1)
plotsp(Xr[s, , drop = FALSE], col = "red")
plotsp(Xr_approx[s, , drop = FALSE], col = "blue", add = TRUE, lty = 2)

Xu_approx <- xfit(fm$Tu, fm$P, fm$xmeans)
dim(Xu_approx)
s <- sample(1:nrow(Xu), size = 1)
plotsp(Xu[s, , drop = FALSE], col = "red")
plotsp(Xu_approx[s, , drop = FALSE], col = "blue", add = TRUE, lty = 2)

}

\keyword{datagen}