## rnirs - Regression, Discrimination and Other Methods for Chemometrics  
## **Version 1.9-0**

### **Keywords**

Regression (R), Discrimination (DA), Factorial Analyses (PCA, PLS, FDA), Latent variables (PCR/DA, PLSR/DA), Ridge R/DA, Non Linear Kernel (KPCR/DA, KPLSR/DA, SVMR/DA), Local and Locally Weighting (KNNR/DA, KNN-LWPLSR/DA), Signal pre-processing, Plotting, Chemometrics, Spectral data

### **News**

See the NEWS of the last version by writing in the R console
```{r}
news(package = "rnirs")
```
### **Dependent packages**

**rnris** is dependent to the following 6 packages available on CRAN:

| Package | Which use in rnris? |
|---|---|
| data.table | Fast data management |
| FNN | Fast search of nearest neighbours |
| kernlab | SVM core algorithms |
| matrixStats | Fast column- and row-wise operations on matrices |
| ptw | ALS detrend algorithm |
| signal | Savitsky-Golay derivation algorithm |

### **Usage**

Load **rnirs**. Write in the R console

```{r}
library(rnirs)
```
**To get an overview of the available functions**

Write in the R console

```{r}
??rnirs
```
### **Contact**

matthieu.lesnoff@cirad.fr


































Package rnirs is a R statistical tool box for chemometrics, but is also generic for any type of data. 

It includes tools for

- data checking, pre-processing and plotting
- PLS and PCA (linear and non linear)
- Regression and discrimination: linear methods, non linear kernel methods (including SVM), locally weighted methods (e.g. LWPLSR and LWPLSDA), robust methods, etc. 

See the **rnirs-package** help page for a detailed list of the functions.

You can install and update the package directly from GitHub (see below), or by asking the .tar.gz or zip installation file (for the successive versions) to the author. See 'Contact' section at the end. 

Using Rstudio is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

The changes in the package versions are reported in the **NEWS** file above. 

If you use package **rnris** and like it, do not hesitate to **mark a star** on this GithHub web-page. You may also register as watchers for receiving alerts for some important updates. For both of these operations, you need to be connected on your account.


## Main features 

* **Data checking and summarizing**
* **Data pre-processing**
    - Detrend
        - Polynomial
        - Lowess
        - ALS
    - Moving average
    - SNV
    - Derivation
        - Savitsky-Golay
        - Finite differences
* **Multivariate factorial analyses** 
    - PLS
        - Usual
        - Robust
    - PCA
        - Usual
        - Robust
    - Non Linear Kernel
        - KPLS (Rosipal & Trejo 2001)
        - KPCA
        - Direct KPLS and KPCA
    - FDA
* **Regression**
    - Linear
        - Multiple Linear Regression (MLR)
        - Ridge Regression (RR)
        - On latent variables
            - PLSR
            - PCR
    - Non Linear Kernel
        - KRR (LS-SVM)
        - INLR
        - On latent variables
            - KPLSR (Rosipal & Trejo 2001)
            - KPCR 
            - Direct KPLSR and KPCR
        - SVM-R
    - Locally Weighted (LW)
        - KNN-Weighted Regression
        - KNN-LWPLSR
        - Generic KNN-LW model building function
* **Discrimination (DA)**
    - Probabilistic 
        - Parametric: LDA, QDA
        - Non-parametric (Gaussian kernel density estimation)
    - Distance based
        - SIMCA-DA
        - DIS-DA
    - Linear 
        - regressions on the Y-Dummy table
            - DA-LM
            - DA-RR
            - DA-GLM
        - On Latent variables
            - PLSDA
            - PCDA 
    - Non Linear Kernel
        - DA-KRR (DA-LS-SVM)
        - On Latent variables
            - KPLSDA
            - KPCDA
            - Direct KPLSDA and KPCDA
        - SVM-C
    - Locally Weighted (LW)
        - KNN-Weighted DA
        - KNN-LWPLSDA
        - Generic KNN-LW model building function
* **Multiblock**
    - Orthogonalization of a matrix to another matrix
    - Block-selection
    - Block-scaling (for MB-PLSR or MB-PLSDA)
    - Block-dimension reduction by PLS or PCA
    - Block-dimension reduction by SO-PLS or SO-PCA
* **Model stacking**
    - Quantitative predictions
    - Classes predictions
* **Cross-validation**
    - Generic function
* **Graphics**
* **Miscellaneous functions**

You can write **??rnirs** in the R console for getting details.

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
- **kernlab**
- **matrixStats**
- **ptw**
- **signal**

B. Then, install the file **rnirs_....tar.gz** (Menu "Packages", "Install", and then "Install From Package Archive Files")


