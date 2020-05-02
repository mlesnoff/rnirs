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

\item{asp}{Scalar. Giving the aspect ratio y/x. The value \code{asp = 0} is the default in \code{\link{plot.default}} (no constraints on the ratio). See \code{\link{plot.default}}.}

\item{col}{A color, or a vector of colors (of length equal to the number of groups), defining the color(s) of the points.}

\item{group}{A vector of length \eqn{n} defining groups of observations to be plotted with different colors (default to \code{NULL}).}

\item{legend}{Only if there are groups. Logical indicationg is a legend is drawn for groups (Default to \code{FALSE}).}

\item{legend.title}{Character string indicationg a title for the legend.}

\item{ncol}{Number of columns drawn in the legend box.}

\item{zeroes}{Logical indicationg if an horizontal and vertical lines are drawn at coordonates (0, 0) (Default to \code{FALSE}).}

\item{circle}{Logical indicating if a correlation circle (see examples in \code{\link{pls}}) is plotted  (default to \code{FALSE}).}

\item{ellipse}{Logical indicating if a Gaussian  ellipse is plotted (default to \code{FALSE}). If there are groups, an ellipse is drawn for each group.}

\item{labels}{Logical indicating if the row names of \code{X} (instead of points) are plotted (default to \code{FALSE}).}

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
cor.circle <- fm$cor.circle
coord.var <- fm$coord.var
headm(Tr)
headm(Tu)

par(mfrow = c(1, 2))
plotxy(Tr[, 1:2])
plotxy(Tr[, 3:4], pch = 16, ellipse = TRUE)
par(mfrow = c(1, 1))

T <- rbind(Tr, Tu)
group <- c(rep("Reference", nrow(Tr)), rep("Unknown", nrow(Tu)))
par(mfrow = c(1, 2))
plotxy(T, group = group, pch = 16, ellipse = TRUE)
plotxy(T[, 3:4], group = group, pch = 16)
par(mfrow = c(1, 1))

### See: ?hcl.colors, hcl.pals()
group <- 1:nrow(Tr)
col <- hcl.colors(n = length(group), alpha = .5, rev = FALSE, palette = "Blues")
plotxy(Tr, group = group, col = col, legend = FALSE, pch = 16)

############## FICTIVE PCA EXAMPLE

n <- 10 ; p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(NULL)
Xr <- X[1:8, ]
Xu <- X[9:10, ] 
fm <- pca(Xr, Xu, ncomp = ncomp)
Tr <- fm$Tr
Tu <- fm$Tu
P <- fm$P

T <- rbind(Tr, Tu)
row.names(T) <- 1:nrow(T)
group <- c(rep("Reference", nrow(Tr)), rep("Unknown", nrow(Tu)))
plotxy(T, group = group, pch = 16, zeroes = TRUE)

plotxy(T, group = group, labels = TRUE, zeroes = TRUE)

plotxy(P, zeroes = TRUE, labels = TRUE)

plotxy(fm$coord.var, zeroes = TRUE, labels = TRUE)

plotxy(fm$cor.circle, zeroes = TRUE, labels = TRUE, circle = TRUE, ylim = c(-1, 1))

\donttest{

############## EXTERNAL RESIZED WINDOW

Xr <- datcass$Xr
fm <- pca(Xr, ncomp = 4)

## Do as below (removing comments '#'):

#windows(width = 7, height = 6)
#plotxy(Tr)

#dev.off()

}




}

\keyword{datagen}