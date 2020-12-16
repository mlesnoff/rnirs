\name{segmkf}
\alias{segmkf}
\alias{segmts}
\encoding{latin1}

\title{Segments for cross-validation}

\description{

Build segments of observations for K-Fold or "test-set" (i.e. Monte Carlo) cross-validation (CV). 

The CV can eventually be randomly repeated. For each repetition:

- Function \code{segmkf} (K-fold CV) returns the \eqn{K} segments.

- Function \code{segmts} (test-set CV) returns a segment (of a given length) randomly sampled in the data set.

\bold{CV of blocks} 

Argument \code{y} allows sampling \bold{blocks of observations} instead of observations. This can be useful for instance when there are repetitions in the data. In such a situation, CV should account for the repetition level (if not, error rates will in general be strongly underestimated). For implementing such a CV, object \code{y} must be a a vector of length \eqn{n} defining the blocks (in the same order as the data).

In any cases (\code{y = NULL} or not), the functions return a list of vector(s). Each vector contains the \bold{indexes of the observations} defining the segment.

}

\usage{

segmkf(n, y = NULL, K = 5, type = c("random", "consecutive", "interleaved"),
  nrep = 1, seed = NULL) 

segmts(n, y = NULL, m, nrep, seed = NULL) 

}

\arguments{

\item{n}{The total number of row observations in the data set. If \code{y = NULL}, the CV is implemented on \code{1:n}. If \code{y != NULL}, blocks of observations (defined in \code{y}) are sampled instead of observations (but indexes of observations are returned).}

\item{y}{A vector of length \eqn{n} defining the blocks. Default to \code{NULL}.}

\item{K}{For \code{segmkf}. The number of folds (i.e. segments) in the K-fold CV.}

\item{type}{For \code{segmkf}. The type K-fold CV. Possible values are "random" (default), "consecutive" and "interleaved".}

\item{m}{For \code{segmts}. If \code{y = NULL}, the number of observations in the segment. If not, the number of blocks in the segment.}

\item{nrep}{The number of replications of the repeated CV. Default to \code{nrep = 1}.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

}

\examples{

######### K-fold

segmkf(n = 10, K = 3)

segmkf(n = 10, K = 3, type = "interleaved")

# Leave-one-out
segmkf(n = 10, K = 10)

# Repeated
segmkf(n = 10, K = 3, nrep = 2)

######### Test-set (repeated)

segmts(n = 10, m = 3, nrep = 5)

######### With blocks

n <- 10
y <- rep(LETTERS[1:5], 2)
y

z <- segmkf(n = n, y = y, K = 3, nrep = 1)
z
y[z$rep1$segm1]
y[z$rep1$segm2]
y[z$rep1$segm3]

z <- segmts(n = n, y = y, m = 3, nrep = 1)
z
y[z$rep1$segm1]

}

\keyword{datagen}