\name{checkdupl}
\alias{checkdupl}
\alias{rmdupl}
\encoding{latin1}

\title{Find or remove duplicated row observations between two data sets}

\description{

Function \code{checkdupl} finds the duplicated row observations between two matrices or data frames.

Function \code{rmdupl} removes the duplicated row observations between two matrices or data frames.

}

\usage{

checkdupl(X, Y, nam = NULL, digits = NULL, check.all = FALSE)

rmdupl(X, nam = NULL, digits = NULL, check.all = FALSE)

}

\arguments{

\item{X}{A matrix or data frame, compared to \code{Y}.}

\item{Y}{A matrix or data frame, compared to \code{X}.}

\item{nam}{The names of the variables to consider in \code{X} and \code{Y}: the test of duplication is undertaken only over the variables in \code{nam}. If \code{NULL} (default), \code{nam} is set to all the column names of \code{X}. The variables set in \code{nam} must be common between \code{X} and \code{Y}.}

\item{digits}{The number of digits used when rounding the variables (set in \code{nam}) before the test. Default to \code{NULL} (no rounding.}

\item{check.all}{Logical (default = \code{FALSE}). If \code{TRUE}, an additionnal test of duplication is undertaken considering all the columns of \code{X} (even if \code{nam} is defined as a part of these columns).}

}

\value{A data frame reporting the duplicated rows.}

\examples{

dat1 <- matrix(c(1:5, 1:5, c(1, 2, 7, 4, 8)), nrow = 3, byrow = TRUE)
dimnames(dat1) <- list(1:3, c("v1", "v2", "v3", "v4", "v5"))

dat2 <- matrix(c(6:10, 1:5, c(1, 2, 7, 6, 12)), nrow = 3, byrow = TRUE)
dimnames(dat2) <- list(1:3, c("v1", "v2", "v3", "v4", "v5"))

dat1
dat2

checkdupl(dat1, dat2)

checkdupl(dat1, dat2, nam = c("v1", "v2"))

checkdupl(dat1, dat2, nam = c("v1", "v2"), check.all = TRUE)

z <- checkdupl(X = dat1, Y = dat1)
z[z$rownum.X != z$rownum.Y, ]

z <- checkdupl(dat1, dat1, nam = c("v1", "v2"))
z[z$rownum.Y != z$rownum.Y, ]

rmdupl(dat1)

rmdupl(dat1, nam = c("v1", "v2"))

}

\keyword{datagen}