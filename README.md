## **rnirs - Regression and Discrimination for Chemometrics**  
## <span style="color:grey70"> **Version 1.9-5** </span> 

## <span style="color:green"> **Key Features** </span> 

- **Factorial analyses** 
  - PCA, PLS, FDA
  - Robust PCA and PLS
  - Non Linear Kernel PCA and PLS

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
  - Local methods: KNNDA, KNN-LWPLSDA

- **Multi-block data reduction**
  - MB-PCA, MB-PLS
  - SOPCA, SOPLS

- **Cross-validation (CV)**

- **Missing data imputation (MDI)**

- **Signal pre-processing**

- **Plotting**


**Click** [**here**](https://github.com/mlesnoff/rnirs/blob/master/doc/rnirs_functions_github.md) **to see the list of the available functions** 

or write in the R console
```{r}
vignette("rnirs_functions")
```

## <span style="color:green"> **News** </span> 

Click [**here**](https://github.com/mlesnoff/rnirs/blob/master/inst/NEWS.md) to see **what changed** in the last version 

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

e.g. with tag "v1.9-0"

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

