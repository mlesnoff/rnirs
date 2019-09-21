\name{plotsp}
\alias{plotsp}
\alias{plotsp1}
\encoding{latin1}

\title{Plotting spectra}

\description{

\code{plotsp} plots lines corresponding to the spectra (or more generally row observations) of a data set.

\code{plotsp1} plots only one line per plot and can be used for scrolling the lines (i.e. spectrum by spectrum). After running a \code{plotsp1} command, the plots are printed successively by pushing the R console "entry button", and stopped by entering any character in the R console.

}

\usage{
plotsp(
  X,
  rownum = NULL, step = NULL, ranges = NULL,
  col = NULL,
  col.low = "#ece7f2",
  col.high = "#045a8d",
  xlim = NULL, ylim = NULL,
  xlab = "x-value", ylab = "y-value", main = NULL,
  add = FALSE,
  text = FALSE,
  ...
  )

plotsp1(X, coord.fixed = FALSE, col = "blue", main = "Row", ...)

}
  
\arguments{

\item{X}{A matrix or data frame of observations (usually spectra). The column names of \eqn{X} must be integers (usually wavelengths).}

\item{rownum}{Vector of the row numbers of the observations in \eqn{X} to be plotted. If \code{NULL} (default), all the observations are plotted.}

\item{step}{Integer representing at which step are selected the columns that are plotted. If \code{NULL} (default), all the columns are plotted.}

\item{ranges}{See \code{\link{selw}}.Default to \code{NULL}: all the columns of \code{X} are considered. If not \code{NULL}, the data are plotted separately for each range.}

\item{col}{Defines the color of the observations. Examples of color names are returned by \code{colors()}. If \code{NULL} (default), a color gradient is defined by function \code{seq_gradient_pal} of package \code{scales} (the gradient is defined by arguments \code{col.low} and \code{col.high} below) (See for instance http://colorbrewer2.org for other gradients than colors()).}

\item{col.low}{Colour for low end of the gradient. See function \code{seq_gradient_pal} of package \code{scales}. Ignored if argument \code{col} is not \code{NULL}.}

\item{col.high}{Colour for high end of the gradient. See function \code{seq_gradient_pal} of package \code{scales}. Ignored if argument \code{col} is not \code{NULL}.}

\item{xlim}{See function \code{\link{plot}}.}

\item{ylim}{See function \code{\link{plot}}.}

\item{xlab}{See function \code{\link{plot}}.}

\item{ylab}{See function \code{\link{plot}}.}

\item{main}{See function \code{\link{plot}}. In \code{plotsp1}, this is a character variable defining the title of the plots with the row number automatically added. If \code{NULL}, there is no title.}

\item{add}{Logical defining if the frame of the plot is plotted (\code{add = FALSE}; default) or not (\code{add = TRUE}). This allows to add new observations to a plot without red-building the frame.}

\item{text}{Logical (default = \code{FALSE}). If \code{TRUE}, the contents of vector \code{rownum} are plotted on the graph.}

\item{...}{Other arguments to pass in function \code{\link{plot}}}.

\bold{Specific argument for} \code{plotsp1}

\item{coord.fixed}{Logical (default to \code{FALSE}). If \code{TRUE}, all the scrolled plots have the same \eqn{y}-scale.}

}

\examples{

data(datcass)

X <- datcass$Xr

plotsp(X, ylab = "Absorbance")
plotsp(X, col.low = "grey80", col.high = "grey60")
plotsp(X, col.low = "grey95", col.high = "orangered")
plotsp(X, col.low = "grey95", col.high = "blue3")
plotsp(X, col.low = "#fff7bc", col.high = "#cc4c02") 
plotsp(X, col.low = "#fee0d2", col.high = "#a50f15")

plotsp(X, col = "red3")

par(mfrow = c(1, 2))
plotsp(X, rownum = c(5, 170), col = "darkblue", text = TRUE)
plotsp(X[c(5, 170), ], col = "darkblue", text = TRUE)
par(mfrow = c(1, 1))

plotsp(X, col = "grey")
plotsp(X, rownum = c(5, 170), col = c("red", "blue"), add = TRUE, text = TRUE, lwd = 2)

ranges <- list(c(1000, 1700), c(1800, 2300))
plotsp(X, ranges = ranges, col = "grey")
plotsp(X[c(5, 170), ], ranges = ranges, col = c("red", "blue"), add = TRUE, text = TRUE)

\donttest{

fm <- pca(X, ncomp = 50)
P <- fm$P

plotsp1(t(P), main = "Loadings")

}

}

\keyword{datagen}