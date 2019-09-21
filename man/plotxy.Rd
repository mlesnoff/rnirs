\name{plotxy}
\alias{plotxy}
\encoding{latin1}

\title{2d scatter plot}

\description{2d scatter plot (e.g. for PCA or PLS scores or loadings).}

\usage{
plotxy(X, label = FALSE, group = NULL, 
  origin = colMeans(X[, 1:2]), circle = FALSE, ellipse = FALSE,
  alpha = 1/5, pal = NULL, labpal = NULL, ...)
}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame. If \eqn{p > 2}, only the first two columns are considered.}

\item{label}{Logical indicating if the row names (instead of points) are plotted (default to \code{FALSE}).}

\item{group}{A vector of length \eqn{n} defining groups of observations to be plotted with different colors (default to \code{NULL}).}

\item{origin}{A two-components numeric vector defining the coordonates of the origin of the central horizontal and vertical axes to be plotted. If \code{NULL}, no central axes are plotted.}

\item{circle}{Logical indicating if a correlation circle is plotted  (default to \code{FALSE}).}

\item{ellipse}{Only considered if \code{group} is not \code{NULL}. Logical indicating if a Gaussian  ellipse is plotted for each group (default to \code{FALSE}).}

\item{alpha}{A value between 0 and 1 defining the transparency of the points (see http://ggplot2.tidyverse.org/reference/geom_point.html). A value \code{alpha = 1} means no transparency. The transparency option is useful when \code{n} is large. Ignored if \code{group} is not \code{NULL}.} 

\item{pal}{A vector defining the palette of the colors to use for the groups. See the examples. If the vector is of length 1, the function expects the name of a Brewer pallette (http://ggplot2.tidyverse.org/reference/scale_brewer.html ; http://colorbrewer2.org). Ignored if \code{group} is \code{NULL}.}

\item{labpal}{The label (legend) of the palette of colors in the graph. Ignored if \code{group} is \code{NULL}.}

\item{...}{Other arguments to pass in \code{\link{geom_point}}. Ignored if \code{group} is not \code{NULL}.}

}

\details{
Function \code{\link{plotxy}} uses package \code{ggplot2} available on CRAN.
}

\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr
Xu <- datcass$Xu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)

ncomp <- 5
res <- pls(Xr, yr, Xu, ncomp = ncomp)
names(res)
Tr <- res$Tr
Tu <- res$Tu
head(Tr)
head(Tu)

comp <- c(1, 2)
#comp <- c(3, 4)
plotxy(Tr[, comp]) 
plotxy(Tr[, comp], alpha = 1) 
plotxy(Tr[, comp], col = "red", alpha = 1) 

plotxy(Tr[, comp], origin = NULL) 

res <- rbind(Tr, Tu)
typ <- c(rep("Reference", nrow(Tr)), rep("Test", nrow(Tu)))
comp <- c(1, 2)
#comp <- c(3, 4)
plotxy(res[, comp], group = typ, origin = c(0, 0)) 

pal <- c("grey50", "red")
plotxy(res[, comp], group = typ, pal = pal, labpal = "Type of \ndata", origin = c(0, 0)) 

pal <- "Dark2"
#pal <- "Set1"
#pal <- c("#fc9272", "#a50f15") # (Reds)
plotxy(res[, comp], group = typ, pal = pal, origin = NULL)   

plotxy(res[, comp], group = typ, origin = c(0, 0), pal = "Dark2", ellipse = TRUE)


}

\keyword{datagen}