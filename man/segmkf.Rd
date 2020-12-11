\name{segmkf}
\alias{segmkf}
\alias{segmts}
\encoding{latin1}

\title{Segments for cross-validation}

\description{

Build segments for two cross-validation strategies: K-Fold CV, and test-set (or Monte Carlo) CV. 

The CV can eventually be randomly repeated. For each repetition:

- Function \code{segmkf} (K-fold CV) returns the \eqn{K} segments.

- Function \code{segmts} (test-set CV) returns a segment (of a given length \eqn{m}) randomly sampled in the data set.


Each segment is a vector of the selected indexes of the observations.

If argument \code{y} (vector of classes of length \eqn{n}) is not \code{NULL}, the CV (K-fold or Test-set) is done within each class first, and then the indexes are gathered in the segment.

}

\usage{

segmkf(n, y = NULL, K = 5, type = c("random", "consecutive", "interleaved"),
  nrep = 1, seed = NULL) 

segmts(n, y = NULL, m, nrep, seed = NULL) 

}

\arguments{

\item{n}{The total number of row observations in the data set. If \code{y = NULL}, the cross-validation is implemented on \code{1:n}. If \code{y != NULL}, the cross-validation is implemented within each class contained in \code{y} (but the functions return the row observations corresponding to the selected classes).}

\item{y}{A vector of classes. Must be of length \eqn{n}). Default to \code{NULL}.}

\item{K}{The number segments in the K-fold CV.}

\item{type}{The type K-fold CV. Possible values are "random" (default), "consecutive" and "interleaved".}

\item{m}{The length of the segment in the test-set CV.}

\item{nrep}{The number of replications of the repeated CV. Default to \code{nrep = 1}.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

}

\examples{

#### K-fold

segmkf(n = 10, K = 3)

segmkf(n = 10, K = 3, type = "interleaved")

# Leave-one-out
segmkf(n = 10, K = 10)

# repeated
segmkf(n = 10, K = 3, nrep = 2)

#### Test-set (repeated)

segmts(n = 10, m = 3, nrep = 5)

#### With classes

n <- 10
y <- rep(LETTERS[1:5], 2)
y

segmkf(n = n, y = y, K = 3, nrep = 1)

segmts(n = n, y = y, m = 3, nrep = 1)

}

\keyword{datagen}