\name{sampclas}
\alias{sampclas}
\encoding{latin1}

\title{Within-class sampling}

\description{

Random sampling row observations of a data set within pre-defined classes.

}

\usage{

sampclas(X, y, m = 1, meth = c("random"))

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame in which row observations are selected.}

\item{y}{A vector of length \eqn{n} defining the class membership of the observations.}

\item{m}{An integer defining the equal number of observation(s) to select per class, or a vector of integers defining the number to select for each class. In the last case, the vector must have a length equal to the number of classes.}

\item{meth}{Method of selection of the observations in each class. In the present version of the function, the only possible value is "random".}

}

\value{
A list with various outputs, such as:
\item{s}{A vector of the row indexes to select in the data \eqn{X}.}
}

\references{

Naes, T., 1987. The design of calibration in near infra-red reflectance analysis by clustering. Journal of Chemometrics 1, 121-134.

}

\examples{

data(datcass)

X <- datcass$Xr

fm <- pca.svd(X, ncomp = 10)
T <- fm$T
nclas <- 3
z <- kmeans(x = T, centers = nclas, nstart = 25, iter.max = 50)
names(z)
y <- z$cluster

u <- sampclas(X = T, y = y, m = 5)
names(u)
u$tab
u$y
u$s
u$y[u$s]

comp <- c(1, 2)
#comp <- c(2, 3)
par(mfrow = c(1, 2))
plot(T[, comp], col = y)
plot(T[, comp], col = "grey")
points(T[u$s, comp], col = u$y[u$s], pch = 19)
par(mfrow = c(1, 1))

u <- sampclas(X = T, y = y, m = c(2, 0, 10))
u$tab
u$s
u$y[u$s]

}

\keyword{datagen}