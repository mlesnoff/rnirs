\name{sampclas}
\alias{sampclas}
\encoding{latin1}

\title{Within-class sampling}

\description{

Stratified sampling of observations on pre-defined classes.

If argument \code{y = NULL} (default), the sampling is random within each class. If not, the sampling is systematic, i.e. not random (grid), over the quantitative variable \eqn{y}.

}

\usage{

sampclas(x, y = NULL, m = 1, seed = NULL)

}

\arguments{

\item{x}{A integer vector of length \eqn{n} defining the class membership of the observations.}


\item{y}{A vector of length \eqn{n} defining the quantitative variable for the systematic sampling. If \code{NULL} (default), the sampling is random.}


\item{m}{Either an integer defining the (equal) number of observation(s) to select per class, or a vector of integers defining the number to select for each class. In the last case, the vector must have a length equal to the number of classes.}

\item{seed}{An integer defining the seed for the random simulations, or \code{NULL} (default). See \code{\link{set.seed}}.}


}

\value{

A vector of the indexes (i.e. component numbers in \eqn{x}) of the selected observations.

}



\references{

Naes, T., 1987. The design of calibration in near infra-red reflectance analysis by clustering. Journal of Chemometrics 1, 121-134.

}

\examples{

x <- sample(c(1, 3, 4), size = 20, replace = TRUE)
x
table(x)

s <- sampclas(x, m = 2, seed = 1)
s
unlist(s)

y <- rnorm(length(x))
sampclas(x, y, m = 2)

########## Representative stratified sampling from an unsupervised clustering

data(datcass)
X <- datcass$Xr
y <- datcass$yr
n <- nrow(X)

fm <- pca.eigenk(X, ncomp = 10)
z <- kmeans(x = fm$T, centers = 3, nstart = 25, iter.max = 50)
x <- z$cluster
z <- table(x)
p <- z / n
p

psamp <- .20
m <- round(psamp * n * p)
m

## Random

s <- sampclas(x, m = m, seed = 1)
s

## Systematic for y

s <- sampclas(x, y, m = m)
s

}

\keyword{datagen}