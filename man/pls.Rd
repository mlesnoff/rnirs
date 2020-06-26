\name{pls}
\alias{pls}
\alias{pca}
\encoding{latin1}

\title{PLS and PCA}

\description{

Partial least squares (PLS) and principal component (PCA) analyses.

Functions \code{pls} fit PLS1 or PLS2 models, and \code{pca} PCA models.

Data are internally centered before the analyses, but they are not column-wise scaled. If needed, the user has to do the scaling before using the functions (there is no argument \code{scale} in the functions).  

}

\usage{

pls(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...)

pca(Xr, Xu = NULL, ncomp, algo = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The number of PCA or PLS scores (i.e. components) to be calculated.}

\item{algo}{For \code{pls}, a function implementing a PLS (default to \code{\link{pls.kernel}}). For \code{pca}, a function (algorithm) implementing a PCA. If \code{NULL} (default): if \eqn{(n < p)} and \eqn{n < 2000}, \code{\link{pca.eigenk}} is used; in the other cases, \code{\link{pca.eigen}} is used. }

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of outputs (see examples), such as:

\item{Tr}{The Xr-score matrix (\eqn{n x ncomp}).}

\item{Tu}{The Xu-score matrix (\eqn{m x ncomp}).}

\item{P}{The Xr-loadings matrix (\eqn{p x ncomp}).}

\item{explvar}{Proportions of \eqn{Xr-}variance explained by the Xr-score space.}

And other outputs: See the examples and function defined in \code{algo}.
}


\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X[1:8, ] ; Yr <- Y[1:8, ] 
Xu <- X[9:10, ] ; Yu <- Y[9:10, ] 

##################### PLS

ncomp <- 3
fm <- pls(Xr, Yr, ncomp = ncomp)
names(fm)
fm$Tr
fm$P

### Proportion of explained X-variance

fm$explvarx

### Projection of Xu on the score space Tr ==> Scores Tu

fm <- pls(Xr, Yr, Xu, ncomp = ncomp)
fm$Tr
fm$Tu

##################### PCA

ncomp <- 3
fm <- pca(Xr, ncomp = ncomp)
names(fm)
fm$Tr   
fm$P

### Proportion of explained X-variance

z <- fm$explvarx
z
barplot(100 * z$pvar, names.arg = paste("comp", z$ncomp), 
  ylab = "Pct. of variance explained")

### Scores

comp <- c(1, 2)
#comp <- c(2, 3)
plotxy(fm$Tr[, comp], zeroes = TRUE)

plotxy(fm$Tr[, comp], zeroes = TRUE, labels = TRUE)

### Contributions of the individuals (in proportions)

fm$contr.ind
colSums(fm$contr.ind)

### Loadings

plotxy(fm$P, zeroes = TRUE, label = TRUE)

### Coordonates of the variables

plotxy(fm$coord.var, zeroes = TRUE, label = TRUE)

### Contributions of the variables (in proportions)

fm$contr.var
colSums(fm$contr.var)

### Correlation circle

fm$cor.circle

plotxy(fm$cor.circle, zeroes = TRUE, labels = TRUE, circle = TRUE, ylim = c(-1, 1))

### Projection of Xu on the score space Tr ==> Scores Tu

fm <- pca(Xr, Xu, ncomp = ncomp)
Tr <- fm$Tr
Tu <- fm$Tu

T <- rbind(Tr, Tu)
row.names(T) <- 1:nrow(T)
group <- c(rep("Reference", nrow(Tr)), rep("Unknown", nrow(Tu)))
plotxy(T, group = group, pch = 16, zeroes = TRUE)
plotxy(T, group = group, labels = TRUE, zeroes = TRUE)

}

\keyword{datagen}