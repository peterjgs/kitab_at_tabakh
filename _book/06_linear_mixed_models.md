# Linear mixed models

##  Finding variance components from lme models



```r
estu <- read.csv("data/Estuaries.csv")

str(estu)
```

```
## 'data.frame':	54 obs. of  7 variables:
##  $ X                   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Modification        : chr  "Modified" "Modified" "Modified" "Modified" ...
##  $ Estuary             : chr  "JAK" "JAK" "JAK" "JAK" ...
##  $ Site                : int  1 1 2 2 3 3 4 4 1 1 ...
##  $ Hydroid             : int  0 0 0 0 1 1 0 0 7 5 ...
##  $ Total               : int  44 42 32 44 42 48 45 34 29 51 ...
##  $ Schizoporella.errata: int  15 8 9 14 6 12 28 1 0 0 ...
```



```r
estu.lme1 <- lme(Total ~ Modification, random = ~ 1 | Estuary,
                 data = estu,
                 correlation = corCompSymm(form = ~ 1 | Estuary))

summary(estu.lme1)
```

```
## Linear mixed-effects model fit by REML
##  Data: estu 
##        AIC      BIC    logLik
##   404.6881 414.4444 -197.3441
## 
## Random effects:
##  Formula: ~1 | Estuary
##         (Intercept) Residual
## StdDev:    7.424348 9.277184
## 
## Correlation Structure: Compound symmetry
##  Formula: ~1 | Estuary 
##  Parameter estimate(s):
## Rho 
##   0 
## Fixed effects: Total ~ Modification 
##                          Value Std.Error DF   t-value p-value
## (Intercept)           40.97295  4.726969 47  8.667912  0.0000
## ModificationPristine -14.47295  6.230091  5 -2.323072  0.0678
##  Correlation: 
##                      (Intr)
## ModificationPristine -0.759
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -2.3859461 -0.7141963  0.2765803  0.5239747  2.0455630 
## 
## Number of Observations: 54
## Number of Groups: 7
```



```r
VarCorr(estu.lme1)
```

```
## Estuary = pdLogChol(1) 
##             Variance StdDev  
## (Intercept) 55.12094 7.424348
## Residual    86.06614 9.277184
```

