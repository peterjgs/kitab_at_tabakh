# Tidyverse


## Stacking and unstacking data

Stack a two-column data frame with `pivot_longer` and then unstack it with `pivot_wider`.

Firstly with `stack` and `unstack`


```r
dd1 <- as.data.frame(matrix(rpois(10, 5), nrow = 5))
dd1
```

```
##   V1 V2
## 1  6 10
## 2  5  5
## 3  3  6
## 4  2  5
## 5  7  4
```



```r
dd2s <- stack(dd1)
dd2s
```

```
##    values ind
## 1       6  V1
## 2       5  V1
## 3       3  V1
## 4       2  V1
## 5       7  V1
## 6      10  V2
## 7       5  V2
## 8       6  V2
## 9       5  V2
## 10      4  V2
```


```r
unstack(dd2s)
```

```
##   V1 V2
## 1  6 10
## 2  5  5
## 3  3  6
## 4  2  5
## 5  7  4
```

Now with `pivot_longer` and  `pivot_wider`.


```r
library(tidyverse)
```

```
## -- Attaching packages ----------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.3     v dplyr   1.0.2
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## v purrr   0.3.4
```

```
## -- Conflicts -------------------------------------------- tidyverse_conflicts() --
## x dplyr::collapse() masks nlme::collapse()
## x dplyr::filter()   masks stats::filter()
## x dplyr::lag()      masks stats::lag()
## x dplyr::select()   masks MASS::select()
```

```r
dd2pl <- dd1 %>%
  pivot_longer(cols = c("V1", "V2"))
dd2pl
```

```
## # A tibble: 10 x 2
##    name  value
##    <chr> <int>
##  1 V1        6
##  2 V2       10
##  3 V1        5
##  4 V2        5
##  5 V1        3
##  6 V2        6
##  7 V1        2
##  8 V2        5
##  9 V1        7
## 10 V2        4
```




```r
dd3pl <- 
  dd2pl %>% 
  group_by(name) %>%
  mutate(row = row_number() ) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  select(-row)

dd3pl
```

```
## # A tibble: 5 x 2
##      V1    V2
##   <int> <int>
## 1     6    10
## 2     5     5
## 3     3     6
## 4     2     5
## 5     7     4
```






