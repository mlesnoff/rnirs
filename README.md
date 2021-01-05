## **rnirs - Dimension reduction, Regression and Discrimination for Chemometrics**  
## <span style="color:grey70"> **Version 1.9-10** </span> 

## <span style="color:green"> **Key Features** </span> 

- **Dimension reduction** 
  - PCA, PLS, FDA
  - Robust PCA and PLS
  - Non Linear Kernel PCA and PLS
  - Multi-block
    - MB-PCA, MB-PLS (block auto-scaling)
    - SO-PCA, SO-PLS (sequential block orthogonalization)

- **Regression**
  - PCR, PLSR
  - Ridge (RR)
  - Robust PCR and PLSR
  - Non Linear Kernels
    - KPCR, KPLSR
    - KRR (LS-SVMR)
    - SVMR
  - Local methods
    - KNNR
    - KNN-LWPLSR 

- **Discrimination**
  - PCDA, PLSDA
  - Ridge (RR)
  - Robust PCDA and PLSDA
  - Non Linear Kernels
    - KPCDA, KPLSDA
    - KRDA (LS-SVMC)
    - SVMC
  - Local methods
    - KNNDA
    - KNN-LWPLSDA

- **Selecting model dimension**
  - Cross-validation
    - PCA
    - Regression or discrimination models
  - Mallows Cp (AIC, AICc, BIC)
  - Model complexity (Degrees of Freedom)
  - Heuristic methods
    - Scree plot, Horn, Kaiser, Karlis, Broken-stick
    - Bootstrap (Loadings stability)
    - Permutation tests

- **Missing data imputation (MDI)**
  - Iterative algorithm
  
- **Signal pre-processing**
  - Detrend (polynom, ALS), Smoothing, SNV, Derivations, etc.

- **Sampling**
  - Kennard-Stone
  - Duplex
  - Classes

- **Plotting**

## <span style="color:green"> **Available functions** </span> 

**Click** [**HERE**](https://github.com/mlesnoff/rnirs/blob/master/doc/rnirs_functions_github.md) **to see the list of the available functions** 

or write in the R console
```{r}
vignette("rnirs_functions")
```

**After the package installation, all the functions have a help page with documented examples**. 

## <span style="color:green"> **News** </span> 

Click [**HERE**](https://github.com/mlesnoff/rnirs/blob/master/inst/NEWS.md) to see **what changed** in the last version 

or write in the R console
```{r}
news(package = "rnirs")
```

## <span style="color:green"> **Dependent packages** </span> 

**rnris** is dependent to the following 6 packages available on CRAN:

| Package | Which use in rnris? |
|---|---|
| data.table | Fast data management |
| FNN | Fast search of nearest neighbours |
| kernlab | SVM core algorithms |
| matrixStats | Fast column- and row-wise operations on matrices |
| ptw | ALS detrend algorithm |
| signal | Savitsky-Golay derivation algorithm |

## <span style="color:green"> **Installation** </span> 

Using [**Rstudio**](https://www.rstudio.com/products/rstudio/download/) is recommended for installation and usage

### <span style="color:green"> 1.  Install package **'remotes'** from CRAN </span>

Use the **Rstudio** menu 

or write in the R console
```{r}
install.packages("remotes")
```

### <span style="color:green"> 2. Install package **'rnirs'** </span> 

**a) Most recent version**

Write in the R console
```{r}
remotes::install_github("mlesnoff/rnirs", dependencies = TRUE, 
  build_vignettes = TRUE)
```
In case of the following question during installation process:
```{r}
These packages have more recent versions available.
Which would you like to update?"
```
it is recommended to skip updates (usually choice **3** = None)

**b) Any given tagged version**

e.g. with tag "v1.9-0"   (Be carefull, this is not the more recent version)

write in the R console
```{r}
remotes::install_github("mlesnoff/rnirs@v1.9-0", dependencies = TRUE, 
  build_vignettes = FALSE)
```

### <span style="color:green"> 3. Usage </span>

Write in the R console
```{r}
library(rnirs)
```

## <span style="color:green"> **Author** </span> 

**Matthieu Lesnoff**

- Cirad, [**UMR Selmet**](https://umr-selmet.cirad.fr/en), Montpellier, France

- [**ChemHouse**](https://www.chemproject.org/ChemHouse), Montpellier

**matthieu.lesnoff@cirad.fr**

### How to cite

Lesnoff, M. 2021. R package rnirs: Dimension reduction, Regression and Discrimination for Chemometrics. https://github.com/mlesnoff/rnirs. CIRAD, UMR SELMET, Montpellier, France






