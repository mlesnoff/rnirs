\name{selcoll}
\alias{selcoll}
\encoding{latin1}

\title{Heuristic selection of the dimension of a PCA or PLS model using collinearity between bootstrapped loading (or b-coefficient) vectors}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA or PLS by bootstrapping the observations and exploring the collinearity of the loading vectors of same rank or, for PLS (univariate), eventually the b-coefficient vectors. 

The principle is detailed below for loading vectors (the same applies to b-coefficient vectors). 

A non parametric bootstrap is implemented on the rows of matrix \eqn{X} (and eventually \eqn{Y} if PLS), and the loading matrices \eqn{P(b)  b = 1,...,B} are calculated for each bootstrap replication \eqn{b}, all with a total number of \eqn{A} columns. For a given model dimension \eqn{a <= A}, the \eqn{B} loading vectors corresponding to loadings "\eqn{a}" (column \eqn{a} in matrices \eqn{P(b)}) are set in a matrix \eqn{V(a)} (this last matrix has \eqn{B} columns). 

Then, two alternative measures of collinearity are proposed, depending on argument \code{corr}:

1) Default method. Correlation coefficients are calculated between couples of columns \eqn{V(a)} and set in a vector \eqn{v}. The non-collinearity indicator \eqn{r} is the quantile of the elements in \eqn{v} (by default, prob = 1, correspondind to \eqn{max(v))}.  

2) A SVD decompostion of \eqn{V(a)} is computed, and the collinearity measure \eqn{r} between the \eqn{B} vectors is given by proportion of variance accounted by the first SVD dimension (i.e. \eqn{r = eig[1] / sum(eigs)}). 

Low collinearity between the vectors of rank \eqn{a} (columns of matrix  matrix \eqn{V(a)}) may indicate they may have built with large uncertainity (generating unstability in \eqn{V(a)}). Jumps in the curve of \eqn{r}, followed by regular patterns are also informative. 

}

\usage{

selcoll(
    X, Y = NULL, ncomp = NULL, algo = NULL,
    B = 50, seed = NULL,
    type = c("P", "b"),
    coll = c("corr", "eig"),
    prob = 1,
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    print = TRUE, 
    ...
    )

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{For PLS, a \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of responses. If \code{NULL} (default) a PCA is implented.}

\item{ncomp}{The maximal number of PCA or PLS scores (= components = latent variables) to be calculated.}

\item{algo}{ For \code{pca}, a function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used. For \code{pls}, a function implementing a PLS. Default to  \code{NULL} (\code{\link{pls_kernel}} is used).}

\item{B}{Number of bootstrap replications.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

\item{type}{Type of output whose the stability is evaluated. Possible values are "P" (loadings; default) and or "b" (b-coefficients).} 

\item{coll}{Type of collinearity measure. Possible values are "corr" (quantile of correlation coefficeints; default) or "eig" (SVD decomposition).} 

\item{prob}{Probability level for quantile (default to 1; the maximal vaule is considered).} 

\item{plot}{Logical. If \code{TRUE} (default), results are plotted.}

\item{xlab}{Label for the x-axis of the plot.}

\item{ylab}{Label for the y-axis of the plot.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list with output \code{r}, see examples.

}

\examples{

data(datcass)
Xr <- datcass$Xr
yr <- datcass$yr

ncomp <- 30
selcoll(Xr, ncomp = ncomp, B = 10)

}

\keyword{datagen}