# **R Package rnirs**

**https://github.com/mlesnoff/rnirs**

## DATA MANAGEMENT 

### <span style="color:green"> Checking </span> 

- **checkdupl**: Find duplicated row observations between two data sets 
- **rmdupl**: Remove duplicated row observations between two data sets
- **checkna**: Find and count NA values in a data set

### <span style="color:green"> Summary </span> 

- **centr**: Centers of classes
- **dtaggregate**: Summary statistics with data subsets
- **summ**: Summary of the variables of a data set

### <span style="color:green"> Preprocessing </span> 

- **dderiv**: Derivation by finite difference
- **detrend**: Detrend transformation (polynom, lowess, als)
- **mavg**: Smoothing by moving average
- **savgol**: Savitsky-Golay filtering (derivation)
- **snv**: Standard-normal-deviation transformation

### <span style="color:green"> Sampling </span> 

- **sampclas**: Within-class (stratified) sampling
- **sampdp**: Duplex sampling 
- **sampks**: Kennard-Stone sampling 

## MULTIVARIATE FACTORIAL ANALYSES

### <span style="color:green"> PCA </span> 

- **pca**: PCA with the available algorithms below

- **Algorithms for usual PCA**

  - pca.eigen: EIGEN decomposition
  - pca.eigenk: EIGEN for wide matrices (kernel form)
  - pca.nipals: NIPALS
  - pca.nipalsna: NIPALS allowing missing data
  - pca.svd: SVD decomposition
  
- **Algorithms for robust PCA**
  
  - pca.cr Projection-Pursuit PCA (Croux & Ruiz-Gazen 2005)
  - pca.rob: Robust weighting
  - pca.sph: Spherical PCA (Locantore 1999) \cr

### <span style="color:green"> PLS </span>

- **pls**: PLS with the available algorithms below.

- **Algorithms for usual PLS**

  - **pls.kernel**: "Improved Kernel #1" (Dayal & McGregor 1997)
  - **pls.nipals**: NIPALS
  - **pls.rannar**: Kernel version for wide matrices (Rannar et al. 1994)

- **Algorithms for robust univariate PLS**

  - **pls.iw**: Iterative re-weighting
  - **pls.rob**: Weighting

### <span style="color:green"> FDA </span>

- **fda**: Using EIGEN decomposition of a compromise
- **fda.svd**: Using weighted SVD decomposition of class centers

### <span style="color:green"> Non Linear Kernel </span>

- **KPCA (Scholkopf et al. 2002)**

  - **kpca**: EIGEN decomposition

- **KPLS (Rosipal & Trejo 2001)**

  - **kpls** (uses **kpls.nipals**)
  - **kpls.nipals**: NIPALS  

- **Direct KPCA and KPLS (Bennett & Embrechts 2003)**

  - **kgram**: Build Gram matrices for direct kernel models
  
- **Available kernel functions**

  - **kpol**: Polynomial
  - **krbf**: Gausssian RBF
  - **ktanh**: Hyperbolic tangent

### <span style="color:green"> Outlyingness Multivariate Measures (Outliers) </span> 

- **out.eucl**: Outlyingness using Euclidean distance
- **out.sdod**: Outlyingness based on a score space
- **out.stah**: Stahel-Donoho outlyingness

### <span style="color:green"> Auxilliary functions </span>

- **scordis, **lscordis**: Score distances for a score space
- **odis**, **lodis**: Orthogonal distances for a score space
- **xfit**: Matrix Approximation from multivariate scores and loadings

## REGRESSION MODELS

### <span style="color:green"> Linear </span> 

- **lmr**: Multiple linear regression
- **rr**: Ridge Regression\cr

- **On latent variables**

  - **pcr**: PCR
  - **plsr**: PLSR
  - **Auxiliary functions**
    - **bcoef**: b-coefficients for PLSR and PCR models
    - Same other auxiliary functions as for PCA/PLS

### <span style="color:green"> Non Linear </span> 

- **inlr**: Blocks for INLR (Berglund & Wold 1997)

- **Kernel**

  - **krr**: Kernel ridge regression (LS-SVM)
  - **svmr**: SVM regression
  - **On latent variables**
    - Using functions **kpca** and **kpls.nipals**
      - **kpcr**: Kernel PCR
      - **kplsr**: Kernel PLSR
    - Direct KPCR and KPLSR
      - **dkplsr**
      - Or use **kgram**
      
- **Locally weighted**

  - **knnr**: KNN regression
  - **lwplsr**: KNN-LWPLSR
  - **Generic function**
    - **locw**: building KNN-locally weighted models
      
## DA MODELS (DISCRIMINATION) 

### <span style="color:green"> DA methods </span> 

- **Probabilistic**

  - **daprob**: Probablistic (parametric and non-parametric) LDA and QDA

- **Distance-based**

  - **dadis**: Using the dissimilarity to the class centers
  - **dasdod**: Using a Simca index

- **Linear on the Y-dummy table**
  - **daglm**: Using generalized linear models
  - **dalm**: Using multivariate linear regression
  - **darr**: Using linear ridge regression
  
### <span style="color:green"> Linear </span> 

- **On latent variables**
  - **pcda**: PCDA using any above DA method
  - **pcdalm**: same as **pcda(dalm)** but faster
  - **plsda**: PLSDA using any above DA method
  - **plsdalm**: same as **plsda(dalm)** but faster
  
### <span style="color:green"> Non Linear </span> 
  
- **Kernel**
  - **dakrr**: Using kernel ridge regression
  - **svmc**: SVM classification
  - **On latent variables**
    - Using functions **kpca** and **kpls.nipals**
      - **kpcda**: Kernel PCDA using any above DA method
      - **kpcdalm**: Same as **kpcda(dalm)** but faster
      - **kplsda**: Kernel PLSDA using any above DA method
      - **kplsdalm**: Same as **kplsda(dalm)** but faster
    - Direct KPCDA and KPLSDA 
      - **dkplsda**
      - **dkplsdalm**
      - Or use **kgram**
 
- **Locally weighted** (see **locw**)

  - **knnda**: KNN DA
  - **lwplsda**: KNN-LWPLSDA

## SELECTION OF VARIABLES 

- **covsel**: COVSEL algorithm

## MULTIBLOCK

- **orthog**: Orthogonalization of a matrix to another matrix
- **blockpls**: Block dimension reduction by PLS
- **blockscal**: Block autoscaling
- **blocksel**: Block selection in a matrix
- **blocksopca**: Block dimension reduction by SO-PCA
- **blocksopls**: Block dimension reduction by SO-PLS

## MODEL TUNING

- **splitpar** Split the value of a parameter within an interval

## MODEL STACKING

- **stackavg**: Stacking for regression models 
- **stackavg.cla**: Stacking for discrimination models

## CROSS-VALIDATION AND ERROR RATES

- **segmkf**: Building segments for K-Fold CV
- **segmts**: Building segments for test-set CV
- **fitcv**: Generic function for cross-validating a model
- **Prediction error rate**
  - **mse**: For regression models
  - **err**: For discrimination models
  - **plotmse**: Plotting error rates of prediction models
  - **selwold**: Wold's criterion for selecting the number of components in PCR/DA and PLSR/DA model

## GRAPHICS

- **plotjit**: Jittered plot
- **plotsp**, **plostsp1**: Plotting spectra, loadings, or more generally row observations of a data set
- **plotxy**: 2-d scatter plot

## AUXILIARY

- **dis**: Dissimilarities between row observations of a matrix and a given vector
- **dkern.gauss**, **dmnorm**: Prediction of probability density of multivariate data 
- **dummy**: Table of dummy variables
- **headm**: Return the first part of a matrix or data frame
- **getknn**: KNN selection
- **matB**, **matW**: Between and within covariance matrices
- **matdis**: Dissimilarity matrix (between observations)
- **pinv**: Moore-Penrose pseudo-inverse
- **sourcedir**: Source every R functions in a directory
- **wdist**: Weights for distances
- See also the **auxiliary functions** in files **z...R**

## AUTHOR

**Matthieu Lesnoff**

- Cirad, UMR Selmet, Montpellier, France

- ChemHouse, Montpellier

**matthieu.lesnoff@cirad.fr**
