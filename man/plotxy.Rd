\name{plotxy}
\alias{plotxy}
\encoding{latin1}

\title{2d scatter plot}

\description{2d scatter plot.}

\usage{
plotxy(X, asp = 1, col = NULL, 
  group = NULL, legend = TRUE, legend.title = NULL, ncol = 1,
  zeroes = FALSE, circle = FALSE, ellipse = FALSE,
  labels = FALSE,
  ...)
}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame. If \eqn{p > 2}, only the first two columns are considered.}

\item{asp}{Scalar. Giving the aspect ratio y/x. See \code{\link{plot.default}}.}

\item{col}{A color, or a vector of colors (of length equal to the number of groups), defining the color(s) of the points.}

\item{group}{A vector of length \eqn{n} defining groups of observations to be plotted with different colors (default to \code{NULL}).}

\item{legend}{Only if there are groups. Logical indicationg is a legend is drawn for groups (Default to \code{FALSE}).}

\item{legend.title}{Character string indicationg a title for the legend.}

\item{ncol}{Number of columns drawn in the legend box.}

\item{zeroes}{Logical indicationg if an horizontal and vertical lines are drawn at coordonates (0, 0) (Default to \code{FALSE}).}

\item{circle}{Logical indicating if a correlation circle (see examples in \code{\link{pls}}) is plotted  (default to \code{FALSE}).}

\item{ellipse}{Logical indicating if a Gaussian  ellipse is plotted (default to \code{FALSE}). If there are groups, an ellipse is drawn for each group.}

\item{labels}{Logical indicating if the row names of \code{FALSE} (instead of points) are plotted (default to \code{FALSE}).}

\item{...}{Other arguments to pass in functions \code{\link{plot}}, \code{\link{points}}, \code{\link{axis}} and \code{\link{text}}.}

}


\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr
Xu <- datcass$Xu
yu <- datcass$yu

ncomp <- 5
fm <- pca(Xr, Xu, ncomp = ncomp)
names(fm)
Tr <- fm$Tr
Tu <- fm$Tu
headm(Tr)
headm(Tu)

par(mfrow = c(1, 2))
plotxy(Tr[, 1:2])
plotxy(Tr[, 3:4])
par(mfrow = c(1, 1))

T <- rbind(Tr, Tu)
group <- c(rep("Reference", nrow(Tr)), rep("Unknown", nrow(Tu)))
plotxy(T, group = group)

windows(width = 7, height = 6)
plotxy(T, zeroes = TRUE, group = group, pch = 16, ellipse = TRUE,
  xlab = "Component 1", ylab = "Component 2")

}

\keyword{datagen}