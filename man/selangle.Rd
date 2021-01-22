\name{selangle}
\alias{selangle}
\encoding{latin1}

\title{Heuristic selection of the dimension of a PCA or PLS model using angles between bootstrapped loading matrices}

\description{

The function helps selecting the dimension (i.e. nb. components) of a PCA or PLS by bootstrapping the loading matrix \eqn{P} and exploring its stability. Stability is quantified by angles between the boostrapped matrices. 

The general idea was proposed by Ye & Weiss 2003 for the sliced inverse regression, and applied to PCA by Luo & Li 2016. The loading matrix \eqn{P} (with a total number of \eqn{A} columns, i.e. loading vectors) is computed on the raw matrix \eqn{X}. 

Then, a non parametric bootstrap is implemented on the rows of matrix \eqn{X}, and the loading matrices \eqn{P(b)  b = 1,...,B} are calculated for each bootstrap replication \eqn{b}, all with \eqn{A} columns. 

For a given model dimension \eqn{a <= A}, an "angle" is then calculated between the raw matrix \eqn{P} and each matrix \eqn{P(b)}, all with considering only the  first \eqn{a} columns. The stability indicator for a matrix  \eqn{P} with \eqn{a} vectors is the mean of the \eqn{B} angles. 

Higher is the mean angle (meaning that the compared matrices do not span the same space), lower is the stability of matrix \eqn{P} whose some last columns were probably built from noise. 

Two measures of angle are proposed, depending on argument \code{angle}

1) Default: The "maxsub" angle (See Krzanowski, 1979, Hubert et al 2005, and Engelen et al. 2005).

2) The vector correlation coefficient "q" (Hotelling 1936) used by Ye & Weiss 2003 and Luo & Li 2016). 

Print function rnirs::.corvec for the formulas.  

Angles are first computed in radians (the right angle = \eqn{pi / 2}), and then divided by \eqn{pi / 2} to vary between 0 and 1 (1 = minimal stability). 

Jumps in the curve of the mean angle, followed by regular patterns are also informative. 

}

\usage{

selangle(
    X, Y = NULL, ncomp = NULL, algo = NULL, 
    B = 50, seed = NULL,
    angle = c("maxsub", "hot"),
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

\item{angle}{Type of angle. Possible values are "maxsub" (default) or "hot" (q of Hotelling).} 

\item{plot}{Logical. If \code{TRUE} (default), results are plotted.}

\item{xlab}{Label for the x-axis of the plot.}

\item{ylab}{Label for the y-axis of the plot.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list with output \code{r} = vector of the standardized angle.

}

\references{

Engelen, S., Hubert, M., Branden, K.V., 2005. A Comparison of Three Procedures for Robust PCA in High Dimensions. Austrian Journal of Statistics 34, 117-126-117-126. https://doi.org/10.17713/ajs.v34i2.405

Hubert, M., Rousseeuw, P.J., Vanden Branden, K., 2005. ROBPCA: A New Approach to Robust Principal Component Analysis. Technometrics 47, 64-79. https://doi.org/10.1198/004017004000000563

Krzanowski, W.J., 1979. Between-Groups Comparison of Principal Components. Journal of the American Statistical Association 74, 703-707. https://doi.org/10.1080/01621459.1979.10481674

Luo, W., Li, B., 2016. Combining eigenvalues and variation of eigenvectors for order determination. Biometrika 103, 875-887. https://doi.org/10.1093/biomet/asw051

Ye, Z., Weiss, R.E., 2003. Using the Bootstrap to Select One of a New Class of Dimension Reduction Methods. Jasa 98, 968-979. https://doi.org/10.1198/016214503000000927

}

\examples{

data(datcass)
Xr <- datcass$Xr
yr <- datcass$yr

ncomp <- 30
selangle(Xr, yr, ncomp = ncomp, B = 10)

}

\keyword{datagen}