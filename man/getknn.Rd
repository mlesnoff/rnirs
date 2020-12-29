\name{getknn}
\alias{getknn}
\encoding{latin1}

\title{kNN selection}

\description{

Function \code{getknn} selects the \eqn{k} nearest neighbours of each row observation of a new (= test) data set within a reference (= training) data set, based on a dissimilarity measure. 

\code{getknn} uses function \code{\link{get.knnx}} of package \code{FNN} (Beygelzimer et al.) available on CRAN.  

}

\usage{

getknn(Xr, Xu, k = NULL,
  diss = c("euclidean", "mahalanobis", "correlation"), 
  algorithm = "brute", list = TRUE)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (test) observations.}

\item{k}{The number of nearest neighbors to select.}

\item{diss}{The type of dissimilarity used between observations. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by \eqn{sqrt(.5 * (1 - rho))}.}

\item{algorithm}{Search algorithm used for Euclidean and Mahalanobis distances. Default to \code{"brute"}. See \code{\link{get.knnx}}.}

\item{list}{If \code{TRUE} (default), a list format is also returned for the outputs.}

}

\value{

A list of outputs, such as:

\item{nn}{A \eqn{n x k} data frame with the row numbers of the neighbors for the observations.Row "i" = indexes of the k nearest neighbors of the test observation "i".}

\item{d}{A \eqn{n x k} data frame with the dissimilarities of the neighbors for the observations. Row "i" = dissimilarities of the k nearest neighbors of the test observation "i".
}

\item{listnn}{Same as \code{$nn} but in a list format.}

\item{listd}{Same as \code{$d} but in a list format.}


}

\examples{

data(datcass)

Xr <- datcass$Xr
Xu <- datcass$Xu

k <- 5
getknn(Xr, Xu[1:3, ], k = k)

z <- pca(Xr, Xu, ncomp = 15)
Tr <- z$Tr
Tu <- z$Tu[1:10, ]
k <- 5
getknn(Tr, Tu, k = k, diss = "mahalanobis")


}

\keyword{datagen}