## **rnirs - Regression, Discrimination and Other Methods for Chemometrics**  
## <span style="color:grey70"> **Version 1.9-0** </span> 

## <span style="color:green"> **Key features** </span> 

**Regression** (R), **Discrimination** (DA), **Factorial Analyses** (PCA, PLS, FDA), **Partial Least Squares**, **Latent variables** (PCR/DA, PLSR/DA), **Ridge R/DA**, **Non Linear Kernel** (KPCA, KPLS, KPCR/DA, KPLSR/DA, SVMR/DA), **Local and Locally Weighting** (KNNR/DA, KNN-LWPLSR/DA), **Signal pre-processing** (derivation, detrend, etc.) , **Cross-validation**, **Plotting**, **Chemometrics**, **Spectral data**

## <span style="color:green"> **News** </span> 

Click [**here**](https://github.com/mlesnoff/rnirs/blob/master/inst/NEWS.md) to see **what changed** in the last version 

or write in the R console
```{r}
news(package = "rnirs")
```

## <span style="color:green"> **Contents**  </span> 

Click [**here**](https://github.com/mlesnoff/rnirs/blob/master/doc/rnirs_functions_github.md) to see the **list of the available functions** 

or write in the R console
```{r}
vignette("rnirs_functions")
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

### <span style="color:green"> 1.  Install package **remotes** from CRAN </span>

Use the **Rstudio** menu 

or write in the R console
```{r}
install.packages("remotes")
```

### <span style="color:green"> 2. Install package **rnirs** </span> 

**Most recent version**

Write in the R console
```{r}
remotes::install_github("mlesnoff/rnirs", dependencies = TRUE)
```
**Any given tagged version**

e.g. with tag "v1.9-0"

write in the R console
```{r}
remotes::install_github("mlesnoff/rnirs@v1.9-0", dependencies = TRUE)
```

### <span style="color:green"> 3. Usage </span>

Write in the R console
```{r}
library(rnirs)
```

## <span style="color:green"> **Author** </span> 

**Matthieu Lesnoff**

- Cirad, UMR Selmet, Montpellier, France

- ChemHouse, Montpellier

**matthieu.lesnoff@cirad.fr**

