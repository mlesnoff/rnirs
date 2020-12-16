\name{sampclas}
\alias{sampclas}
\encoding{latin1}

\title{Within-class sampling}

\description{

The function divides the data \eqn{x} in two sets, "train" vs "test", using a atratified sampling on pre-defined classes.

If argument \code{y = NULL} (default), the sampling is random within each class. If not, the sampling is systematic (regular grid) over the quantitative variable \eqn{y}.

}

\usage{

sampclas(x, y = NULL, k, seed = NULL)

}

\arguments{

\item{x}{A vector of integers of length \eqn{n} defining the class membership of the observations.}

\item{y}{A vector of length \eqn{n} defining the quantitative variable for the systematic sampling. If \code{NULL} (default), the sampling is random within each class.}

\item{k}{Either an integer defining the (equal) number of training observation(s) to select per class, or a vector of integers defining the numbers to select for each class. In the last case, vector \code{k} must have a length equal to the number of classes present in vector \code{x}, and be ordered in the same way as the ordered class membership.}

\item{seed}{An integer defining the seed for the random simulations, or \code{NULL} (default). See \code{\link{set.seed}}.}


}

\value{

Vectors of the indexes (i.e. position in \eqn{x}) of the selected observations.

}



\references{

Naes, T., 1987. The design of calibration in near infra-red reflectance analysis by clustering. Journal of Chemometrics 1, 121-134.

}

\examples{

x <- sample(c(1, 3, 4), size = 20, replace = TRUE)
x
table(x)

z <- sampclas(x, k = 2, seed = 1)
z
x[z$train]

z <- sampclas(x, k = c(1, 2, 1), seed = 1)
z
x[z$train]

y <- rnorm(length(x))
z <- sampclas(x, y, k = 2)
z
x[z$train]

########## Representative stratified sampling from an unsupervised clustering

data(datcass)
X <- datcass$Xr
y <- datcass$yr
n <- nrow(X)

fm <- pca_eigenk(X, ncomp = 10)
z <- kmeans(x = fm$T, centers = 3, nstart = 25, iter.max = 50)
x <- z$cluster
z <- table(x)
p <- z / n
p

psamp <- .70
k <- round(psamp * n * p)
k

## Random

z <- sampclas(x, k = k, seed = 1)
z

## Systematic for y

z <- sampclas(x, y, k = k)
z

}

\keyword{datagen}