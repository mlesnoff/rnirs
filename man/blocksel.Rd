\name{blocksel}
\alias{blocksel}
\encoding{latin1}

\title{Block selection in a matrix}

\description{

Function \code{blocksel} selects blocks (i.e. sets of columns) in a matrix and concatenates the selected blocks into a new matrix. 

}

\usage{

blocksel(X, blocks)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{blocks}{A list of same length as the number of blocks. Each component of the list gives the column numbers in \eqn{X} defining the given block.}

}

\value{

\item{X}{A matrix with the concatenated blocks.}

\item{blocks}{A list of column numbers defining the blocks in the output matrix \code{X}.}

}

\examples{

n <- 3 ; p <- 10
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
colnames(X) <- paste("Var", 1:p, sep = "")
set.seed(NULL)
X

blocks <- list(1:2, 4, 6:8)
blocks

blocksel(X, blocks) 

}

\keyword{datagen}