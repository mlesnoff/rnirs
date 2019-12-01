\name{centr}
\alias{centr}
\encoding{latin1}

\title{Centers of classes}

\description{
Calculation of the centers (eg. mean or median) of classes of row observations of a data set. 
}

\usage{centr(X, Y = NULL, FUN = mean, ...)}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame (usually spectra) on which are calculated the centers.}

\item{Y}{A vector of length \eqn{n} defining the class membership of the row observations. Default to \code{NULL} (all the rows of are considered).}

\item{FUN}{The function used in the aggregation (Default to mean).}

\item{...}{Optionnal arguments to pass in the function defined in \code{FUN}.}

}


\details{\code{centr} uses function \code{\link{aggregate}}.}



\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y <- sample(1:2, size = n, replace = TRUE)
set.seed(NULL)

centr(X, y)
centr(X, y, FUN = median)

data(datforages)

Xr <- datforages$Xr
yr <- datforages$yr
table(yr)

u <- centr(Xr, yr)$centers
v <- centr(Xr, yr, FUN = median)$centers
headm(u)
headm(v)

par(mfrow = c(1, 2))
plotsp(u, col = 1:4, main = "Mean spectra per group")
plotsp(v, col = 1:4, main = "Median spectra per group")
par(mfrow = c(1, 1))

x <- Xr[1:20, ]
plotsp(x, ylab = "Absorbance", col = "grey")
u <- centr(x)$centers
plotsp(u, col = "red", add = TRUE, lwd = 2)

}

\keyword{datagen}