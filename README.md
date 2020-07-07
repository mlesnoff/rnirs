## rnirs 1.7-6
## Chemometrics, Locally Weighted Regressions, Kernel regressions and Other Methods  

Package rnirs is a tool box for chemometrics, including spectra pre-processing and plotting, PLS and PCA regression and discrimination, robust methods, locally weighted methods, kernel methods, etc.

The package is generic for any type of data, although it was initially implemented for near infrared spectral data (NIRS).

The changes in the package versions are reported in the **NEWS** file above. 

Using Rstudio is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

You can install and update the package directly from GitHub (see below), or by asking the .tar.gz or zip installation file (for the successive versions) to the author. See Contact section at the end. 

If you use package **rnris** and like it, do not hesitate to **mark a star** on this GithHub web-page. You may also register as watchers for receiving alerts for some important updates. For both of these operations, you need to be connected on your account.

## Main features 

* Data checking and summarizing
* Data pre-processing
    - Detrend
        - Polynomial
        - Lowess
        - ALS
    - SNV
    - Derivation
        - Savitsky-Golay
        - Finite differences
* Multivariate factorial analyses 
    - PLS
    - PCA
        - Usual
        - Robust
    - FDA
    - Non linear kernel
        - KPCA
* Regression
    - Linear
        - Multiple Linear Regression (MLR)
        - Ridge Regression
        - PLSR
        - PCR
    - Non linear kernel
        - Kernel ridge regression
        - KPCR
        - INLR
    - Locally weighted (wrappers using locw)
        - KNN-R
        - KNN-LWPLSR
* Discrimination
    - DA (various discrimination methods)
    - Linear
        - PLSDA
        - PCDA
    - Non linear kernel
        - KPCDA
    - Locally weighted (wrappers using locw)
        - KNN-DA
        - KNN-LWPLSDA
* Multiblock
    - Orthogonalization of a matrix to another matrix
    - Block-selection
    - Block-scaling
    - Block-dimension reduction by PLS or PCA
    - Block-dimension reduction by SO-PLS or SO-PCA
* MOdel stacking
    - stackavg
    - stackavg.cla
* Cross-validation
    - Generic function **fitcv**
* Graphics
* Miscellaneous functions

You can write **??rnirs** in the R console for details.

## Installation from Github

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

## Installation from the archive file rnirs_....tar.gz with Rstudio

A. First, install the following packages from the CRAN

- **data.table**
- **FNN**
- **matrixStats**
- **ptw**
- **signal**

B. Then, install the file **rnirs_....tar.gz** (Menu "Packages", "Install", and then "Install From Package Archive Files")

## Usage

Load **rnirs**. Write in the R console

```{r}
library(rnirs)
```
**To get an overview of the available functions**

Write in the R console

```{r}
??rnirs
```
### Contact

Matthieu Lesnoff
matthieu.lesnoff@cirad.fr

