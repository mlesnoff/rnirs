\name{fda}
\alias{fda}
\alias{fdasvd}
\encoding{latin1}

\title{Factorial discriminant analysis}

\description{
Functions \code{fda} and \code{fdasvd} fit a factorial discriminant analysis (FDA). The functions maximize the compromise \eqn{p'Bp / p'Wp}, i.e. \eqn{max p'Bp} with constraint \eqn{p'Wp = 1}. Vectors \eqn{p} are the linear discrimant coefficients "LD".

Function \code{fda} uses an eigen decomposition of \eqn{W^(-1)B} (if \eqn{W} is singular, W^(-1) can be replaced by a pseudo-inverse, with argument \code{pseudo}), and function \code{fdasvd} a weighted SVD decomposition of the matrix of the class centers. See the code for details.

}

\usage{

fda(Xr, Yr, Xu = NULL, ncomp = NULL, pseudo = FALSE)

fdasvd(Xr, Yr, Xu = NULL, ncomp = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n} of reference (= training) responses (class membership).} 

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference FDA score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The maximum number of components to consider (\eqn{ncomp <= min(p, nb. classes - 1)}).}

\item{pseudo}{For function \code{fda}. Logical indicating if matrix \eqn{W} has to be inverted by pseudo-inverse. Default to \code{FALSE}.}

\item{...}{For function \code{fdasvd}. Optionnal arguments to pass in function \code{\link{pca}}.}

}

\value{

A list of outputs (see examples), such as:

\item{Tr}{The Xr-score matrix (\eqn{n x ncomp}).}

\item{Tu}{The Xu-score matrix (\eqn{m x ncomp}).}

\item{Tcenters}{The Xr-class centers score matrix (\eqn{nb. classes x ncomp}).}

\item{P}{The Xr-loadings matrix (\eqn{p x ncomp}). Coefficients of linear discriminants: P = "LD" of \code{\link{lda}} of package \code{MASS}.}

\item{explvar}{Proportions of explained variance by PCA of the class centers in transformed scale.}

And other outputs ("z" outputs relate on transformed data; see the code).

}

\references{

Saporta G., 2011. Probabilités analyse des données et statistique. Editions Technip, Paris, France.

}

\examples{

data(iris)

Xr <- iris[, 1:4]
yr <- iris[, 5]

fm <- fda(Xr, yr)
names(fm)

# Xr-scores
headm(fm$Tr)

# Xr-class centers scores
fm$Tcenters

# Xr-loadings matrix
# = coefficients of linear discriminants
# = "LD" of function lda of MASS package
fm$P

# Explained variance by PCA of the class centers 
# in transformed scale
fm$explvar

plotxy(fm$Tr, group = yr, ellipse = TRUE)
points(fm$Tc, pch = 8, col = "blue")

# Object Tcenters is the projection of the class centers in the score space
fm <- fda(Xr, yr)
fm$Tcenters
centers <- centr(Xr, yr)$centers
centers
fm <- fda(Xr, yr, centers)
fm$Tu

}

\keyword{datagen}