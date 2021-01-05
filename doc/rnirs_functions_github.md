# **R Package rnirs**

**https://github.com/mlesnoff/rnirs**

## DIMENSION REDUCTION

### <span style="color:green"> PCA </span> 

- ***pca*** PCA with the available algorithms below

- **Algorithms for usual PCA**

  - ***pca_eigen*** EIGEN decomposition
  - ***pca_eigenk*** EIGEN for wide matrices (kernel form)
  - ***pca_svd*** SVD decomposition
  - ***pca_nipals*** NIPALS
  - ***pca_nipalsna*** NIPALS allowing missing data
  
- **Algorithms for robust PCA**
  
  - ***pca_sph*** Spherical PCA (Locantore 1999) \cr
  - ***pca_cr*** Projection-Pursuit PCA (Croux & Ruiz-Gazen 2005)
  - ***pca_rob*** Robust weighting

### <span style="color:green"> PLS </span>

- ***pls*** PLS with the available algorithms below.

- **Algorithms for usual PLS**

  - ***pls_kernel*** "Improved Kernel #1" (Dayal & McGregor 1997)
  - ***pls_nipals*** NIPALS
  - ***pls_rannar*** Kernel version for wide matrices (Rannar et al. 1994)

- **Algorithms for robust PLS1**

  - ***pls_iw*** Iterative re-weighting
  - ***pls_rob*** Weighting

### <span style="color:green"> FDA </span>

- ***fda*** Using EIGEN decomposition of the compromise "inter/intra"
- ***fdasvd*** Using weighted SVD decomposition of class centers

### <span style="color:green"> Non Linear Kernel </span>

- **KPCA (Scholkopf et al. 2002)**

  - ***kpca*** EIGEN decomposition

- **KPLS (Rosipal & Trejo 2001)**

  - ***kpls*** (uses ***kpls_nipals***)
  - ***kpls_nipals*** NIPALS  

- **Direct KPCA and KPLS (Bennett & Embrechts 2003)**

  - ***kgram*** Build Gram matrices for direct kernel models
  
- **Available kernel functions**

  - ***kpol*** Polynomial
  - ***krbf*** Gausssian RBF
  - ***ktanh*** Hyperbolic tangent

### <span style="color:green"> Multi-block </span> 

- ***blockscal*** Block autoscaling: Used for MB-PLS and MB-PCA 
- ***blockpls*** Block dimension reduction by PLS
- ***blocksopca*** Block dimension reduction by SO-PCA (sequential orthogonalization)
- ***blocksopls*** Block dimension reduction by SO-PLS (sequential orthogonalization)

- ***blocksel*** Block selection in a matrix
- ***orthog*** Orthogonalization of a matrix to another matrix

### <span style="color:green"> Outlyingness Multivariate Measures </span> 

- ***outeucl*** Outlyingness using Euclidean distance
- ***outsdod*** Outlyingness based on a score space
- ***outstah*** Stahel-Donoho outlyingness

### <span style="color:green"> Auxilliary functions </span>

- ***scordis***, ***lscordis*** Score distances for a score space
- ***odis***, ***lodis*** Orthogonal distances for a score space
- ***xfit*** Matrix fitting from multivariate scores and loadings, 
- ***xssr*** SSR calculation for a matrix fitting

## REGRESSION

### <span style="color:green"> Linear </span> 

- ***lmr*** Multiple linear regression
- ***rr*** Ridge Regression

- **On latent variables**

  - ***pcr*** PCR
  - ***plsr*** PLSR
  - **Auxiliary functions**
    - ***bcoef*** b-coefficients for PLSR and PCR models
    - Same other **auxiliary functions** as for PCA & PLS

### <span style="color:green"> Non Linear </span> 

- ***inlr*** Blocks for INLR (Berglund & Wold 1997)

- **Kernel**

  - ***krr*** Kernel ridge regression (KRR = LS-SVM)
  - ***svmr*** SVM regression
  - **On latent variables**
    - Using functions ***kpca*** and ***kpls_nipals***
      - ***kpcr*** Kernel PCR
      - ***kplsr*** Kernel PLSR
    - Direct KRR, KPCR and KPLSR
      - ***dkplsr***
      - Or use ***kgram***
      
- **Locally weighted**

  - ***knnr*** KNN regression
  - ***lwplsr*** KNN-LWPLSR
  - **Generic function**
    - ***locw*** Build any KNN-locally weighted models
      
## DISCRIMINATION (DA) 

### <span style="color:green"> DA methods </span> 

- **Probabilistic**

  - ***daprob*** Probabilistic (parametric and non-parametric) LDA and QDA

- **Distance-based**

  - ***dadis*** Using the dissimilarity to the class centers
  - ***dasdod*** Using a Simca index

- **Linear on the Y-dummy table**
  - ***daglm*** Using generalized linear models
  - ***dalm*** Using multivariate linear regression
  - ***darr*** Using linear ridge regression
  
### <span style="color:green"> Linear </span> 

- **On latent variables**
  - ***pcda*** PCDA using any above DA method
  - ***pcdalm*** same as ***pcda(dalm)*** but faster
  - ***plsda*** PLSDA using any above DA method
  - ***plsdalm*** same as ***plsda(dalm)*** but faster
  
### <span style="color:green"> Non Linear </span> 
  
- **Kernel**
  - ***dakrr*** Using kernel ridge regression (KRR-DA = LS-SVM-DA)
  - ***svmc*** SVM classification
  - **On latent variables**
    - Using functions ***kpca*** and ***kpls_nipals***
      - ***kpcda*** Kernel PCDA using any above DA method
      - ***kpcdalm*** Same as ***kpcda(dalm)*** but faster
      - ***kplsda*** Kernel PLSDA using any above DA method
      - ***kplsdalm*** Same as ***kplsda(dalm)*** but faster
    - Direct KRR-DA, KPCDA and KPLSDA 
      - ***dkplsda***
      - ***dkplsdalm***
      - Or use ***kgram***
 
- **Locally weighted** (see ***locw***)

  - ***knnda*** KNN DA
  - ***lwplsda*** KNN-LWPLSDA

## MODEL STACKING

- ***stackavg*** Stacking for regression models 
- ***stackavgcla*** Stacking for discrimination models

## SELECTING MODEL DIMENSION

### <span style="color:green"> Cross-validation </span> 

- **Generic for regression and DA**

  - ***segmkf*** Building segments for K-Fold CV
  - ***segmts*** Building segments for test-set CV
  - ***cvfit*** Generic function for cross-validating any model
    
- **Specific for PCA**
  - ***cvpca_ia*** IA algorithm
  - ***cvpca_tri*** ekf-TRI algorithm
  - ***cvpca_ckf*** ckf-TRI algorithm
  - ***cvpca_rw*** Row-wise naïve algorithm

### <span style="color:green"> Model complexity (*df*) </span> 

- ***dfpca_div*** SURE estimation of *df* for PCA 
- ***dfplsr_cov*** Bootstrap estimation of *df* for PLSR1
- ***dfplsr_div*** SURE estimation of *df* for PLSR1

### <span style="color:green"> Mallows Cp (AIC, AICc, BIC) </span> 
  
- ***cppca*** Cp criteria for PCA
- ***cpplsr*** Cp criteria for PLSR1

### <span style="color:green"> Heuristic methods </span> 

- ***selscree*** Scree plots for PCA and PLS
- **Specific to PCA**
  - ***selkaiser*** Guttman-Kaiser method (Guttman 1954, Kaiser 1960)
  - ***selkarlis*** Modified Guttman-Kaiser method (Karlis et al. 2003)
  - ***selbroken*** Broken-stick method (Frontier 1976)
  - ***selhorn*** Horn method for PCA (Horn 1965)
- **Stability of PCA and PLS loadings (and PLS *b*-coefficients)**
  - ***selangle*** Angles between bootstrapped loading matrices
  - ***selcoef*** Significant coefficients of bootstrapped loading vectors
  - ***selcoll*** Collinearity of bootstrapped loading (or b-coefficients) vectors 
- **Specific to regression models** 
  - ***selsign*** Permutation tests on CV-PRESS (e.g. van der Voet 1994) 
  - ***selwik*** Permutation test for PLS scores (Wiklund et al. 2007, Faber et al. 2007)
  - ***selwold*** Wold's criterion for PCR, PLSR, PCDA, PLSDA, etc.

### <span style="color:green"> Prediction error rate </span> 
  
- ***mse*** For regression models
- ***err*** For discrimination models
- ***plotmse*** Plotting error rates of prediction models

## SELECTION OF VARIABLES 

- ***covsel*** COVSEL algorithm (Roger et al. 2011)

## MODEL TUNING

- ***splitpar*** Split the value of a parameter within an interval (auxiliary for RR, SVMR, etc.)

## DATA PRE-PROCESSING 

- ***dderiv*** Derivation by finite difference
- ***detrend*** Detrend transformation (polynom, lowess, als)
- ***mavg*** Smoothing by moving average
- ***savgol*** Savitsky-Golay filtering (derivation)
- ***snv*** Standard-normal-deviation transformation

## DATA SAMPLING 

- ***sampks*** Kennard-Stone sampling 
- ***sampdp*** Duplex sampling 
- ***sampclas*** Within-class (stratified) sampling

## DATA MANAGEMENT

### <span style="color:green"> Checking </span> 

- ***checkna*** Find and count NA values in a data set
- ***plotxna*** Plotting missing data in a matrix
- ***checkdupl*** Find duplicated row observations between two data sets 
- ***rmdupl*** Remove duplicated row observations between two data sets

### <span style="color:green"> Summary </span> 

- ***centr*** Centers of classes
- ***dtaggregate*** Summary statistics with data subsets
- ***summ*** Summary of the variables of a data set

## MISSING DATA IMPUTATION

- ***ximputia*** PCA iterative algorithm (IA)

## OTHER GRAPHICS

- ***plotsp*** Plotting spectra, loadings, or more generally row observations of a data set
- ***plostsp1*** Same as  ***plotsp*** but one-by-one row
- ***plotxy*** 2-d scatter plot
- ***plotjit*** Jittered plot
- ***plotsl*** Plotting slopes of successive values (auxiliary)

## AUXILIARY

- ***dis*** Dissimilarities between row observations of a matrix and a given vector
- ***dkerngauss***, ***dmnorm*** Prediction of probability density of multivariate data 
- ***dummy*** Table of dummy variables
- ***headm*** Return the first part of a matrix or data frame
- ***getknn*** KNN selection
- ***matB***, ***matW*** Between and within covariance matrices
- ***matdis*** Dissimilarity matrix (between observations)
- ***pinv*** Moore-Penrose pseudo-inverse
- ***sourcedir*** Source every R functions in a directory
- ***wdist*** Weights for distances
- See also the ***auxiliary functions*** in files **z...R**

## AUTHOR

**Matthieu Lesnoff**

- Cirad, [**UMR Selmet**](https://umr-selmet.cirad.fr/en), Montpellier, France

- [**ChemHouse**](https://www.chemproject.org/ChemHouse), Montpellier

**matthieu.lesnoff@cirad.fr**

### How to cite

Lesnoff, M. 2021. R package rnirs: Dimension reduction, Regression and Discrimination for Chemometrics. https://github.com/mlesnoff/rnirs. CIRAD, UMR SELMET, Montpellier, France


