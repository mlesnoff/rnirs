\name{fda}
\alias{fda}
\encoding{latin1}

\title{Factorial discriminant analysis}

\description{
Function \code{fda} fits a factorial discriminant analysis (FDA). The function maximizes the compromise \eqn{p'Bp / p'Wp}, i.e. \eqn{max p'Bp} with constraint \eqn{p'Wp = 1}. Vectors \eqn{p} (linear discrimant coefficients "LD") are the eigenvectors of \eqn{W^(-1)B}, i.e. \eqn{W^(-1)B * p = lambda * p}. 

Basically, the function undertakes a PCA on the \eqn{n x p} matrix \eqn{Xc} of the class centers with the metric \eqn{W^(-1)}: the first step of the calculation is a Cholesky decomposition \eqn{W^(-1) = U'U} and the second step a SVD of \eqn{Z = Xc * U'}.
}

\usage{fda(Xr, Yr, Xu = NULL, ncomp = NULL)}

\arguments{

\item{Xr}{A \eqn{n x p} matrix of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n} of reference (= training) responses (class membership).} 

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference FDA score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The maximum number of components to consider (\eqn{ncomp <= min(p, nb. classes - 1)}).}

}

\references{

Saporta, G., 2011. Probabilités analyse des données et statistique. Editions Technip, Paris, France.

}

\examples{

library(ggplot2)
data(iris)

Xr <- iris[, 1:4]
yr <- iris[, 5]

fm <- fda(Xr, yr)
names(fm)
fm$P
head(fm$Tr)
fm$Tc
fm$explvar

p <- plotxy(fm$Tr, group = yr)
p <- p + geom_point(
  data = data.frame(fm$Tc), aes(x = comp1, y = comp2),
  pch = 8, cex = 4, col = "blue"
  )
p

# Object Tc is the projection of the class centers in the score space
centers <- centr(Xr, yr)$centers
centers
fm <- fda(Xr, yr, centers)
fm$Tc
fm$Tu

}

\keyword{datagen}