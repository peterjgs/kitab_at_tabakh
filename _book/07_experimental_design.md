# Experimental design


## Sample size by simulation

From: Study Design, 2hr - Gordana


<br>

Using pilot data, find values of linear model parameters. In this experiment, I would like to see if there is an effect of Petal length on Sepal length. I have pilot data with 15 observations.



```r
data(iris)

str(iris)
```

```
## 'data.frame':	150 obs. of  5 variables:
##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```


```r
table(iris$Species)
```

```
## 
##     setosa versicolor  virginica 
##         50         50         50
```



Get some pilot data (or get different data from Gordana's)


```r
# rand_setosa <- sample(1:50, size = 15, replace = F)

# pilot <- iris[rand_setosa, ]

##  Gordana's data

pilot <- iris[1:15, c(1, 3)]
```



```r
pilot_mod <- lm(Sepal.Length ~ Petal.Length, data = pilot)

summary(pilot_mod)
```

```
## 
## Call:
## lm(formula = Sepal.Length ~ Petal.Length, data = pilot)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.50175 -0.25965 -0.05965  0.14825  1.01404 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    4.0912     1.0441   3.918  0.00176 **
## Petal.Length   0.5789     0.7316   0.791  0.44296   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4034 on 13 degrees of freedom
## Multiple R-squared:  0.04595,	Adjusted R-squared:  -0.02743 
## F-statistic: 0.6262 on 1 and 13 DF,  p-value: 0.443
```

```r
beta_0 <- coef(pilot_mod)[1] # estimated intercept (from pilot) 

sd <- summary(pilot_mod)$sigma # estimated variability

my_range <- range(pilot$Petal.Length) # range of petal length

nsim <- 500 # number of simulated datasets
```


Then specify a meaningful effect size (here it’s the slope of Petal length)



```r
# ecologically meaningful slope (same scale as data)

beta_1 <- 1
```



```r
N <- 20 # desired sample size

sim_dat = data.frame(Sepal.Length = NA, 
                     Petal.Length = seq(my_range[1], 
                                        my_range[2], 
                                        length = N) )

pval = rep(NA, nsim)

for (i in 1:nsim) {
  mean_y <- beta_0 + beta_1 * sim_dat$Petal.Length
  sim_dat$Sepal.Length <- rnorm(N, mean = mean_y, sd = sd)
  m <- lm(Sepal.Length ~ Petal.Length, data = sim_dat)
  pval[i] <- coef(summary(m))["Petal.Length", "Pr(>|t|)"]
  } # cycle through all N values

sum(pval < 0.05) / nsim
```

```
## [1] 0.46
```

