\name{segmkf}
\alias{segmkf}
\alias{segmts}
\encoding{latin1}

\title{Segments for cross-validation}

\description{

Build a list of test segments for cross-validation (CV), eventually (randomly) repeated. Each segment is a vector of indexes of row observations (unless argument \code{y} is used).

- Function \code{segmkf} returns the \eqn{K} segments of a K-fold CV.

- Function \code{segmts} returns a segment (of length \eqn{m}) randomly sampled in the data set. This corresponds to a Monte Carlo CV.

}

\usage{

segmkf(n, y = NULL, K = 5, type = c("random", "consecutive", "interleaved"),
  nrep = 1, seed = NULL) 

segmts(n, y = NULL, m, nrep, seed = NULL) 

}

\arguments{

\item{n}{The number of row observations in the data set. If \code{y = NULL}, the cross-validation is implemented on \code{1:n}. If \code{y != NULL}, the cross-validation is implemented on the classes contained in \code{y} (but the functions return the row observations corresponding to the selected classes).}

\item{y}{A vector of classes to be cross-validated instead of the row observations. Default to \code{NULL}.}

\item{K}{The number segments in the K-fold CV.}

\item{type}{The type K-fold CV. Possible values are "random" (default), "consecutive" and "interleaved".}

\item{m}{The length of the segment in the Monte Carlo CV.}

\item{nrep}{The number of replications of the repeated CV.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

}

\examples{

#### K-fold

segmkf(n = 10, K = 3)

segmkf(n = 10, K = 3, type = "interleaved")

# Leave-one-out
segmkf(n = 10, K = 10)

# replicated
segmkf(n = 10, K = 3, nrep = 2)

#### Monte Carlo

segmts(n = 10, m = 3, nrep = 5)

#### With classes

n <- 10
y <- rep(LETTERS[1:5], 2)
y

segmkf(n = n, y = y, K = 3, nrep = 1)

segmts(n = n, y = y, m = 3, nrep = 1)

}

\keyword{datagen}