\name{rnirs-pkg}
\alias{rnirs-pkg}
\docType{package}
\encoding{latin1}

\title{Chemometrics Methods and Locally Weighted PLS Regressions}

\description{

Package rnirs focuses on chemometrics methods, including locally weighted PLS regressions and discriminations.

The package was initially implemented for near infrared spectral (NIRS) data, but is generic for any type of data.

}

\details{

\bold{------------ DATA CHECKING}
\tabular{ll}{
  \code{\link{checkdupl}}, \code{\link{rmdupl}} \tab Find or remove duplicated row observations between two data sets\cr
  \code{\link{checkna}} \tab Find and count NA values in a data set\cr
  }

\bold{------------ DATA SUMMARIZE}
\tabular{ll}{
  \code{\link{centr}} \tab Centers of classes\cr
  \code{\link{dtaggregate}} \tab Summary statistics with data subsets\cr
  \code{\link{summ}} \tab Summary of the variables of a data set\cr
  }

\bold{------------ DATA PREPROCESSING}
\tabular{ll}{
  \code{\link{dderiv}} \tab Derivation by finite difference\cr
  \code{\link{detrend}} \tab Detrend transformation (polynom, lowess, als)\cr
  \code{\link{mavg}} \tab Smoothing by moving average \cr
  \code{\link{savgol}} \tab Savitsky-Golay filtering (derivation)\cr
  \code{\link{snv}} \tab Standard-normal-deviation transformation\cr
  }

\bold{------------ PCA}
\tabular{ll}{
  \code{\link{pca}} \tab PCA with the available algorithms below\cr
  }

\eqn{Usual algorithms}

\tabular{ll}{
  \code{\link{pca.svd}} \tab SVD \cr
  \code{\link{pca.eigen}} \tab Eigen  \cr
  \code{\link{pca.nipals}} \tab NIPALS \cr
  \code{\link{pca.nipalsna}} \tab NIPALS allowing missing data \cr
  }

\eqn{Robust algorithms}

\tabular{ll}{
  \code{\link{pca.sph}} \tab Spherical (Locantore 1999) \cr
  \code{\link{pca.cr}} \tab Croux & Ruiz-Gazen (2005) \cr
  \code{\link{pca.rob}} \tab Robust \cr
  }

\bold{------------ PLS}
\tabular{ll}{
  \code{\link{pls}} \tab PLS with the available algorithms below\cr
  }

\eqn{Algorithms}

\tabular{ll}{
  \code{\link{pls.kernel}} \tab Kernel (faster) \cr
  \code{\link{pls.nipals}} \tab NIPALS \cr
  }

\bold{------------ FDA}

\tabular{ll}{
  \code{\link{fda}} \tab Factorial discriminant analysis\cr
  }

\bold{------------ AUXILLIARY FUNCTIONS FOR PCA, PLS, FDA}

\tabular{ll}{
  \code{\link{scordis}}, \code{\link{lscordis}} \tab Score distances for a score space\cr
  \code{\link{odis}}, \code{\link{lodis}} \tab Orthogonal distances for a score space\cr
  \code{\link{xfit}} \tab Matrix Approximation from multivariate scores and loadings \cr
  \code{\link{bcoef}} \tab b-coefficients for a PLS model\cr
  \code{\link{influ.pls}} \tab Regression influence measures for a PLS model\cr
  }

\bold{------------ REGRESSION}

\eqn{Global (eventually weighted)}

\tabular{ll}{
  \code{\link{lmr}} \tab LMR\cr
  \code{\link{plsr}} \tab PLSR\cr
  }

\eqn{Locally weighted}

\tabular{ll}{
  \code{\link{knnr}} \tab KNNWR \cr
  \code{\link{lwplsr}} \tab LWPLSR, KNN-LWPLSR\cr
  }

\bold{------------ DISCRIMINATION}

\eqn{Methods}

\tabular{ll}{
  \code{\link{dalm}} \tab DA using multivariate linear regression\cr
  \code{\link{dadis}} \tab DA using the dissimilarity to class centers\cr
  \code{\link{daglm}} \tab DA using generalized linear models\cr
  \code{\link{dasdod}} \tab DA using a Simca index\cr
  \code{\link{daprob}} \tab Probablistic DA (parametric and non-parametric LDA and QDA)\cr
  }

\eqn{Global (eventually weighted)}

\tabular{ll}{
  \code{\link{plsda}}, \code{\link{plsdalm}} \tab PLSDA\cr
  }

\eqn{Locally weighted}

\tabular{ll}{
  \code{\link{knnda}} \tab KNNWDA \cr
  \code{\link{lwplsr}} \tab LWPLDA, KNN-LWPLSDA\cr
  }

\bold{------------ VARIABLE SELECTION}
\tabular{ll}{
  \code{\link{covsel}} \tab CovSel\cr
  }

\bold{------------ MULTIBLOCK FUNCTIONS}

\tabular{ll}{
  \code{\link{orthog}} \tab Orthogonalization of a matrix to another matrix\cr
  \code{\link{blocksel}} \tab Block selection in a matrix\cr
  \code{\link{blockscal}} \tab Block scaling\cr
  \code{\link{blockpls}} \tab Block dimension reduction by PLS or PCA\cr
  \code{\link{blocksopls}}, \code{\link{blocksopca}} \tab Block dimension reduction by SO-PLS or SO-PCA\cr
  }

\bold{------------ MODEL STACKING}
\tabular{ll}{
  \code{\link{stackavg}} \tab Model stacking for regression models\cr 
  \code{\link{stackavg.cla}} \tab Model stacking for discrimination models\cr 
  }

\bold{------------ CROSS-VALIDATION AND ERROR RATES}
\tabular{ll}{
  \code{\link{mse}}, \code{\link{err}} \tab Error rates of prediction models\cr 
  \code{\link{segmcvkfold}}, \code{\link{segmcvmc}} \tab Segments for cross-validation \cr
  \code{\link{selncomp.wold}} \tab Selection of the number of components in a PCR/PLSR or PLSDA model \cr
  \code{\link{fitcv}} \tab Cross-validation of a prediction model \cr
  }

\bold{------------ GRAPHICS}

\tabular{ll}{
  \code{\link{plotxy}} \tab 2d scatter plot\cr
  \code{\link{plotmse}} \tab Plotting error rates of prediction models\cr 
  \code{\link{plotsp}}, \code{\link{plotsp1}} \tab Plotting spectra (or more generally row observations) of a data set\cr
  }

\bold{------------ MISCELLANEOUS}

\tabular{ll}{
  \code{\link{headm}} \tab Return the first part of a matrix or data frame \cr
  \code{\link{kenston}} \tab Kennard-Stone sampling \cr
  \code{\link{pinv}} \tab Moore-Penrose pseudo-inverse \cr
  \code{\link{dummy}} \tab Table of dummy variables \cr
  \code{\link{matdis}} \tab Dissimilarity matrix (between observations)\cr
  \code{\link{dis}}  \tab Dissimilarities between a given vector and row observations  \cr
  \code{\link{wdist}} \tab Weights for distances \cr
  \code{\link{sampclas}} \tab Within-class sampling\cr
  \code{\link{getknn}} \tab Knn selection \cr
  \code{\link{locw}} \tab Locally weighted models \cr
  }

\eqn{-- Other}

\tabular{ll}{
  \code{\link{dkern.gauss}}, \code{\link{dmnorm}} \tab Probability density prediction \cr
  
  \code{\link{matB}}, \code{\link{matW}} \tab Between and within covariance matrices \cr


  \code{\link{sourcedir}} \tab Source R functions in a directory \cr
  }

\bold{See also the auxiliary functions in file} \code{zfunctions.R}.

}

\author{
  Matthieu Lesnoff <matthieu.lesnoff@cirad.fr>\cr
  Maintainer: Matthieu Lesnoff <matthieu.lesnoff@cirad.fr>
  }
  
\keyword{package}
