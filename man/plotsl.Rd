\name{plotsl}
\alias{plotsl}
\encoding{latin1}

\title{Plot of slopes of between elemnts of a vector}

\description{

Plot the slopes or differences between two successive elements of a vector \code{x}.

The function is mainly used by other functions, such as \code{\link{selscree}}, etc.

}

\usage{

plotsl(x,
    start = 1,
    plot = c("slope", "diff", "none"),
    xlab = "Index", ylab = "Value",
    ...)

}

\arguments{

\item{x}{A numeric vector.}

\item{start}{Starting value for indexing the elements of vector \code{x} (default to 1).}

\item{plot}{Type of plot. Possible values are \code{"slope"} (slopes), \code{"diff"} (differences) and \code{"none"} (no plot; only the output dataframe is returned).}

\item{xlab}{x-axis label of the plot of \eqn{r} (left-side in the graphic window).}

\item{ylab}{y-axis label of the plot of \eqn{r} (left-side in the graphic window).}

\item{...}{Optionnal arguments to pass through function \code{\link{plot}}.}

}

\value{

A list (invisible) of outputs, see the examples.

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
plotsp(zX)

ncomp <- 30
fm <- pca_eigen(zX, ncomp = ncomp)
plotsl(fm$eig[seq(ncomp)], plot = "diff")

z <- plotsl(fm$eig[seq(ncomp)], plot = "none")
names(z)

}

\keyword{datagen}