\name{pls}
\alias{pls}
\alias{pca}
\encoding{latin1}

\title{PCA and PLS}

\description{

Principal component (PCA) and partial least squares (PLS) analyses.

Function  \code{pca} fits PCA models.

Functions \code{pls} fit PLS1 or PLS2 models. 

Matrix \eqn{X} is centered before the analyses, but \eqn{X} is not column-wise scaled (there is no argument \code{scale} available). If a scaling is needed, the user has to scale \eqn{X} before using the functions. 

Row observations can eventually be weighted with a priori weights (using argument \code{weights}).

}

\usage{

pca(Xr, Xu = NULL, ncomp, algo = NULL, ...)

pls(Xr, Yr, Xu = NULL, ncomp, algo = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The maximal number of PCA or PLS scores (= components = latent variables) to be calculated.}

\item{algo}{For \code{pca}, a function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used. For \code{pls}, a function implementing a PLS. Default to  \code{NULL} (\code{\link{pls_kernel}} is used). }

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

##################### PCA

ncomp <- 3
fm <- pca(Xr, ncomp = ncomp)
names(fm)
fm$Tr   
fm$P

### Proportion of explained X-variance

z <- fm$explvar
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
plotxy(T, group = group, labels = TRUE, zeroes = TRUE, alpha.f = 1)

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

}

\keyword{datagen}