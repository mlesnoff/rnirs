\name{selw}
\alias{selw}
\encoding{latin1}

\title{Selection of column ranges in a data set}

\description{Selection of range(s) of columns in a data set. Usually, selection of range(s) of wavelengths in a spectral data. The function builds a sub-matrix for each range and, in addition, concatenates the sub-matrices.}

\usage{selw(X, ranges)}

\arguments{

\item{X}{A matrix or data frame of observations. The column names of \eqn{X} must be integers (usually representing wavelengths).}

\item{ranges}{A list of defining the ranges to select in \code{X}. Each component of the list defines a range, and must be a vector with 2 components: the first component defines the column name (integer) of the beginning of the range, and the 2nd component defines the column name (integer) of the end of the range.
}

}

\value{
\item{X}{A matrix built by the concatenation of the selected subsets.}
\item{Xlist}{A list of the selected subsets.}
}

\examples{
data(datcass)

X <- datcass$Xr

ranges <- list(c(1000, 1003), c(1800, 1805), c(3000, 4000))
z <- selw(X[1:5, ], ranges)
names(z)
z$X
z$Xlist
}

\keyword{datagen}