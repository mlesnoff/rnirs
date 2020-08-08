\name{rnirs-pkg}
\alias{rnirs-pkg}
\docType{package}
\encoding{latin1}

\title{Chemometrics, Kernel Regression and Discrimination, Locally Weighting, and Other Methods}

\description{

Package rnirs is a tool box for chemometrics, including spectra pre-processing and plotting, data checking, PLS and PCA regression and discrimination, kernel methods, locally weighted methods, robust methods, etc.

The package was initially implemented for near infrared spectral data (NIRS), but is generic for any type of data.

See https://github.com/mlesnoff/rnirs for last version and installation. 

}

\details{

\bold{*************************************** DATA MANAGEMENT}

------------ \bold{Checking}

\tabular{ll}{
  \code{\link{checkdupl}}, \code{\link{rmdupl}} \tab Find or remove duplicated row observations between two data sets\cr
  \code{\link{checkna}} \tab Find and count NA values in a data set\cr
  }

------------ \bold{Summary}

\tabular{ll}{
  \code{\link{centr}} \tab Centers of classes\cr
  \code{\link{dtaggregate}} \tab Summary statistics with data subsets\cr
  \code{\link{summ}} \tab Summary of the variables of a data set\cr
  }

------------ \bold{Preprocessing}

\tabular{ll}{
  \code{\link{dderiv}} \tab Derivation by finite difference\cr
  \code{\link{detrend}} \tab Detrend transformation (polynom, lowess, als)\cr
  \code{\link{mavg}} \tab Smoothing by moving average \cr
  \code{\link{savgol}} \tab Savitsky-Golay filtering (derivation)\cr
  \code{\link{snv}} \tab Standard-normal-deviation transformation\cr
  }

------------ \bold{Sampling}

\tabular{ll}{
  \code{\link{kenston}} \tab Kennard-Stone sampling \cr
  \code{\link{sampclas}} \tab Within-class sampling\cr
  }

\bold{*************************************** MULTIVARIATE FACTORIAL ANALYSES}

------------ \bold{PLS}

\tabular{ll}{
  \code{\link{pls}} \tab PLS with the available algorithms below\cr
  }

\eqn{** Usual}

\tabular{ll}{
  \code{\link{pls.kernel}} \tab "Improved Kernel #1" (Dayal & McGregor 1997) \cr
  \code{\link{pls.nipals}} \tab NIPALS \cr
  \code{\link{pls.rannar}} \tab Kernel version for wide matrices (Rannar et al. 1994) \cr
  }

\eqn{** Robust}

\tabular{ll}{
  \code{\link{pls.rob}} \tab Robust PLS1 (robust PCA and weighted PLS)\cr
  \code{\link{pls.iw}} \tab Robust PLS1 (Iterative Re-Weighting) \cr
  }

------------ \bold{Non Linear Kernel PLS}

\eqn{** Using the KPLS NIPALS algorithm (Rosipal & Trejo 2001)}

\tabular{ll}{
  \code{\link{kpls.nipals}} \tab KPLS NIPALS algorithm)\cr
  \code{\link{kpls}} \tab KPLS (using kpls.nipals)\cr
  }

\eqn{** Direct KPLS}

\tabular{ll}{
  \code{\link{kgram}} \tab Build kernel Gram matrices for direct kernel models \cr
  }

\eqn{** Available kernel functions}

\tabular{ll}{
  \code{\link{kpol}} \tab Polynomial \cr
  \code{\link{krbf}} \tab Gausssian RBF \cr
  \code{\link{ktanh}} \tab Hyperbolic tangente \cr
  }

------------ \bold{PCA}

\tabular{ll}{
  \code{\link{pca}} \tab PCA with the available algorithms below\cr
  }

\eqn{** Usual}

\tabular{ll}{
  \code{\link{pca.svd}} \tab SVD \cr
  \code{\link{pca.eigen}} \tab Eigen  \cr
  \code{\link{pca.eigenk}} \tab Eigen for wide matrices \cr
  \code{\link{pca.nipals}} \tab NIPALS \cr
  \code{\link{pca.nipalsna}} \tab NIPALS allowing missing data \cr
  }

\eqn{** Robust}

\tabular{ll}{
  \code{\link{pca.rob}} \tab Robustly weighted PCA \cr
  \code{\link{pca.cr}} \tab Croux & Ruiz-Gazen PP-PCA (2005) \cr
  \code{\link{pca.sph}} \tab Spherical PCA (Locantore 1999) \cr
  }

------------ \bold{Non Linear Kernel PCA}

\tabular{ll}{
  \code{\link{kpca}} \tab KPCA with various kernel functions (Scholkopf et al. 2002)\cr
  }

\eqn{** Direct KPCA}

\tabular{ll}{
  Use \code{\link{kgram}}
  }

------------ \bold{FDA}

\tabular{ll}{
  \code{\link{fda}}, \code{\link{fda.svd}} \tab Factorial discriminant analysis\cr
  }

------------ \bold{Outlyingness measures}

\tabular{ll}{
  \code{\link{out.stah}} \tab Stahel-Donoho outlyingness \cr
  \code{\link{out.eucl}} \tab Outlyingness using Euclidean distance \cr
  \code{\link{out.sdod}} \tab Outlyingness based on a score space \cr
  }

------------ \bold{Auxilliary functions}

\tabular{ll}{
  \code{\link{scordis}}, \code{\link{lscordis}} \tab Score distances for a score space\cr
  \code{\link{odis}}, \code{\link{lodis}} \tab Orthogonal distances for a score space\cr
  \code{\link{xfit}} \tab Matrix Approximation from multivariate scores and loadings \cr
  }

\bold{*************************************** REGRESSION}

------------ \bold{Linear}

\tabular{ll}{
  \code{\link{lmr}} \tab Linear Regression (MLR)\cr
  \code{\link{rr}} \tab Linear Ridge Regression\cr
  }

\eqn{** On latent variables}

\tabular{ll}{
  \code{\link{plsr}} \tab PLSR\cr
  \code{\link{pcr}} \tab PCR\cr
  }

\eqn{** Auxilliary function}

\tabular{ll}{
  \code{\link{bcoef}} \tab b-coefficients for a PLSR or PCR model\cr
  }

------------ \bold{Non Linear Kernel}

\tabular{ll}{
  \code{\link{krr}} \tab Kernel ridge regression (LS-SVM)\cr
  \code{\link{inlr}} \tab Blocks for INLR (Berglund & Wold 1997)\cr
  }
  
\eqn{** On latent variables (using kpls.nipals and kpca)}

\tabular{ll}{
  \code{\link{kplsr}} \tab Kernel PLSR\cr
  \code{\link{kpcr}} \tab Kernel PCR\cr
  }

\eqn{** Direct KPLSR and KPCR}

\tabular{ll}{
  \code{\link{dkplsr}} \tab Direct Kernel PLSR\cr
  Or use \code{\link{kgram}}
  }

------------ \bold{Locally weighted}

\tabular{ll}{
  \code{\link{knnr}} \tab KNN-WR \cr
  }

\eqn{** On latent variables}

\tabular{ll}{
  \code{\link{lwplsr}} \tab LW-PLSR and KNN-LW-PLSR\cr
  }

\eqn{** Generic function for building KNN-LW regression models}

\tabular{ll}{
  \code{\link{locw}} \tab Locally weighted models\cr
  }

\bold{*************************************** DISCRIMINATION}

------------ \bold{Discrimination methods (DA)}

\eqn{** DA using Regressions on the Y-Dummy table}

(The column of the fitted Y-dummy table with the maximum predicted value
is taken as the predicted class)

\tabular{ll}{
  \code{\link{dalm}} \tab DA using multivariate linear regression\cr
  \code{\link{darr}} \tab DA using linear ridge regression\cr
  \code{\link{dakrr}} \tab DA using non linear kernel ridge regression (LS-SVM)\cr
  \code{\link{daglm}} \tab DA using generalized linear models\cr
  }

\eqn{** Other DA methods}

\tabular{ll}{
  \code{\link{daprob}} \tab Probablistic DA (parametric and non-parametric LDA and QDA)\cr
  \code{\link{dadis}} \tab DA using the dissimilarity to the class centers\cr
  \code{\link{dasdod}} \tab DA using a Simca index\cr
  }

------------ \bold{Linear}

\eqn{** On latent variables}

\tabular{ll}{
  \code{\link{plsda}} \tab PLSDA\cr
  \code{\link{plsdalm}} \tab PLSDA (with dalm)\cr
  \code{\link{pcda}} \tab PCDA\cr
  \code{\link{pcdalm}} \tab PCDA (with dalm)\cr
  }

------------ \bold{Non Linear Kernel}

\eqn{** On latent variables (using kpls.nipals and kpca)}

\tabular{ll}{
  \code{\link{kplsda}} \tab Kernel PLSDA\cr
  \code{\link{kplsdalm}} \tab Kernel PLSDA (with dalm)\cr
  \code{\link{kpcda}} \tab Kernel PCDA\cr
  \code{\link{kpcdalm}} \tab Kernel PCDA (with dalm)\cr
  }

\eqn{** Direct KPLSDA and KPCDA}

\tabular{ll}{
  Use \code{\link{kgram}}
  }

------------ \bold{Locally weighted}

\tabular{ll}{
  \code{\link{knnda}} \tab KNN-WDA \cr
  }

\eqn{** On latent variables}

\tabular{ll}{
  \code{\link{lwplsda}} \tab LW-PLSDA and KNN-LW-PLSDA\cr
  \code{\link{lwplsdalm}} \tab LW-PLSDA and KNN-LW-PLSDA (with dalm)\cr
  }

\eqn{** Generic function for building KNN-LW discrimination models}

\tabular{ll}{
  \code{\link{locw}} \tab Locally weighted models\cr
  }

\bold{*************************************** VARIABLE SELECTION}

\tabular{ll}{
  \code{\link{covsel}} \tab CovSel\cr
  }

\bold{*************************************** MULTIBLOCK FUNCTIONS}

\tabular{ll}{
  \code{\link{orthog}} \tab Orthogonalization of a matrix to another matrix\cr
  \code{\link{blocksel}} \tab Block selection in a matrix\cr
  \code{\link{blockscal}} \tab Block scaling\cr
  \code{\link{blockpls}} \tab Block dimension reduction by PLS or PCA\cr
  \code{\link{blocksopls}}, \code{\link{blocksopca}} \tab Block dimension reduction by SO-PLS or SO-PCA\cr
  }

\bold{*************************************** MODEL STACKING}
\tabular{ll}{
  \code{\link{stackavg}} \tab Stacking for regression models\cr 
  \code{\link{stackavg.cla}} \tab Stacking for discrimination models\cr 
  }

\bold{*************************************** CROSS-VALIDATION AND ERROR RATES}

\tabular{ll}{
  \code{\link{segmcvkfold}}, \code{\link{segmcvmc}} \tab Segments for cross-validation \cr
  \code{\link{fitcv}} \tab Cross-validation of a prediction model \cr
  \code{\link{mse}}, \code{\link{err}} \tab Error rates of prediction models\cr 
  \code{\link{selncomp.wold}} \tab Selection of the number of components in a PLSR/DA or PCR/DA model \cr
  }

\bold{*************************************** GRAPHICS}

\tabular{ll}{
  \code{\link{plotxy}} \tab 2d scatter plot\cr
  \code{\link{plotmse}} \tab Plotting error rates of prediction models\cr 
  \code{\link{plotsp}}, \code{\link{plotsp1}} \tab Plotting spectra (or more generally row observations) of a data set\cr
  }

\bold{*************************************** MISCELLANEOUS}

\tabular{ll}{
  \code{\link{dis}}  \tab Dissimilarities between row observations of a matrix and a given vector  \cr
  \code{\link{dummy}} \tab Table of dummy variables \cr
  \code{\link{headm}} \tab Return the first part of a matrix or data frame \cr
  \code{\link{getknn}} \tab Knn selection \cr
  \code{\link{locw}} \tab Locally weighted models \cr
  \code{\link{matdis}} \tab Dissimilarity matrix (between observations)\cr
  \code{\link{pinv}} \tab Moore-Penrose pseudo-inverse \cr
  \code{\link{wdist}} \tab Weights for distances \cr
  }

\eqn{** Other}

\tabular{ll}{
  \code{\link{dkern.gauss}}, \code{\link{dmnorm}} \tab Probability density prediction \cr
  
  \code{\link{matB}}, \code{\link{matW}} \tab Between and within covariance matrices \cr


  \code{\link{sourcedir}} \tab Source R functions in a directory \cr
  }

\bold{See also the auxiliary functions in files} \code{zfunctions....R}.

}

\author{
  Matthieu Lesnoff <matthieu.lesnoff@cirad.fr>\cr
  Maintainer: Matthieu Lesnoff <matthieu.lesnoff@cirad.fr>
  }
  
\keyword{package}
