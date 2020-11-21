## NEWS for package rnirs

## **Version 1.9-1**

#### **Modification of functions**

- **selwold**: The function has changed. Only vectors are allowed as input. More generic. New plots.




## **Version 1.9-0**

#### **Renamed functions**

- **kentson** to **sampks**
  
- **selncomp.wold** to  **selwold**  (with slight modification)
  
- **segmcvkfold** to  **segmkf**                   
- **segmcvmc** to  **segmts**                   

#### **New functions**

- **plotjit**: Jittered plot
  
- **sampdp**: Duplex sampling algorithm 
  
#### **New arguments**

- Argument **ncomp** added to functions **scordis** and **odis**
  
- Argument **alpha.f** (color opacity for points) added function **plotxy**
  
- Argument **weights** added to function **daglm**
  
- Argument **alpha** in functions **pca.rob** and **pls.rob** has changed of definition (was **1 - alpha** before)

#### **New outputs**

- Output **eig** in functions **pca.eigen**, etc. now contains the full set of eigenvalues (not only the first **ncomp** values)
  
- Output **explvarx** in functions **pca** and **kpca** has been renamed to **explvar**
    
#### **Other**

- Function **sampclas** has changed

- Re-organisation of some outputs in discrimination functions




## **Version 1.8-7**

#### **New functions**

- **svmr**: SVM regression
  
- **svmc**: SVM discrimination (classification)
  
- **splitpar**: Auxiliary function for tuning models








