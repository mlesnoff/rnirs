\name{dummy}
\alias{dummy}
\encoding{latin1}

\title{Table of dummy variables}

\description{

Build a table of dummy variables from a qualitative variable. A binary variable (i.e. 0/1) is created for each level of the qualitative variable. 

}

\usage{dummy(Y)}

\arguments{

\item{Y}{A vector representing a qualitative variable.}

}

\value{A matrix of dummy variables (i.e. binary variables), each representing a given level of the qualitative variable.}


\examples{

y <- c(1, 1, 3, 2, 3)
dummy(y)

y <- c("B", "a", "B")
dummy(y)

}

\keyword{datagen}