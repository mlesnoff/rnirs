## rnirs - Regression, Discrimination and Other Methods for Chemometrics  
## <span style="color:grey40"> **Version 1.9-0** </span> 

### <span style="color:green"> **Keywords** </span> 

**Regression** (R), **Discrimination** (DA), **Factorial Analyses** (PCA, PLS, FDA), **Latent variables** (PCR/DA, PLSR/DA), **Ridge R/DA**, **Non Linear Kernel** (KPCR/DA, KPLSR/DA, SVMR/DA), **Local and Locally Weighting** (KNNR/DA, KNN-LWPLSR/DA), **Signal pre-processing**, **Plotting**, **Chemometrics**, **Spectral data**

### <span style="color:green"> **News** </span> 

See the **NEWS** of the last version by writing in the R console
```{r}
news(package = "rnirs")
```

### <span style="color:green"> **List of the functions** </span> 

See the **list of the availble functions** by writing in the R console
```{r}
vignette("rnirs_functions")
```

### <span style="color:green"> **Dependent packages** </span> 

**rnris** is dependent to the following 6 packages available on CRAN:

| Package | Which use in rnris? |
|---|---|
| data.table | Fast data management |
| FNN | Fast search of nearest neighbours |
| kernlab | SVM core algorithms |
| matrixStats | Fast column- and row-wise operations on matrices |
| ptw | ALS detrend algorithm |
| signal | Savitsky-Golay derivation algorithm |

### <span style="color:green"> **Contact** </span> 

Matthieu Lesnoff

- Cirad, UMR Selmet, Montpellier, France

- ChemHouse, Montpellier

**matthieu.lesnoff@cirad.fr**

### <span style="color:green"> **Installation** </span> 

You can install and update the package directly from GitHub, or by asking the .tar.gz or zip installation file (for the successive versions) to the author. 

Using Rstudio is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

#### **From Github**

**Warning:** The new R base version 4.0.0 requires re-installing all the packages. The easiest way is to remove all the folders where have been installed the packages by the previous version, and then open Rstudio, going in the Package menu and re-installing them automatically with dependencies.

**A. Only for the first installation and if not already done** 

**1. Install Rstudio**

https://rstudio.com/products/rstudio/download/ 

**2. Install the R package devtools from CRAN.** 

Package devtools is used for installing other R packages (such as rnirs) from Github.  

Use the Rstudio menu or write in the R console
```{r}
install.packages("devtools")
```

**3. Install Rtools**

The Rtools program is required for running package devtools.

Two situations:

a. For R version >= 4.0.0

- Download Rtools40.exe at https://cran.r-project.org/bin/windows/Rtools/ and run it.

- Create a text file ".Renviron" (if not already existing) in your "My Documents" folder and copy it the following line
```{r}
PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
```

b. For R versions <= 3.4.6

Download the Rtools.exe available at https://cran.r-project.org/bin/windows/Rtools/history.html and run it.

**B. After this first installation** 

**Load devtools and install rnirs package** 

Write in the R console
```{r}
library(devtools)
install_github("mlesnoff/rnirs", dependencies = TRUE)
```

In case of the following question during installation
```{r}
These packages have more recent versions available.
Which would you like to update?"
```
it is recommended to skip updates.

#### **From an archive file rnirs_....tar.gz with Rstudio**

A. First, install the following packages from the CRAN

- **data.table**
- **FNN**
- **kernlab**
- **matrixStats**
- **ptw**
- **signal**

B. Then, install the file **rnirs_....tar.gz** (Menu "Packages", "Install", and then "Install From Package Archive Files")


