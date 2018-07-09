
### Why sparse matrix?

-   `XGBoost` only works with matrices that contain **all numeric** variables; consequently, we need to one hot encode our data. There are different ways to do this in R. ([UC Business Analytics R Programming Guide](http://uc-r.github.io/gbm_regression))
-   `caret::preProcess`, bagged regression trees for missing values recovery ([Yevhen Vasylenko](https://rpubs.com/geka/BagImput_air)), which requires **all numeric** variables.

### Data

``` r
library(tidyverse)
dd <- data.frame(a = gl(3,4), 
                 b = gl(4,1,12), 
                 c = 1:12, 
                 d = sample(c("X", "Y", "Z"), 12, replace = TRUE))
str(dd)
```

    ## 'data.frame':    12 obs. of  4 variables:
    ##  $ a: Factor w/ 3 levels "1","2","3": 1 1 1 1 2 2 2 2 3 3 ...
    ##  $ b: Factor w/ 4 levels "1","2","3","4": 1 2 3 4 1 2 3 4 1 2 ...
    ##  $ c: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ d: Factor w/ 3 levels "X","Y","Z": 1 2 2 1 1 3 2 1 3 1 ...

The above data frame contains 4 columns (variables): 3 factor and 1 numeric. Multiple packages in R have similar functions on generating sparse matrix or dummy variables.

#### `base::model.matrix`

``` r
(mm0 <- model.matrix(~ . , data = dd))
```

    ##    (Intercept) a2 a3 b2 b3 b4  c dY dZ
    ## 1            1  0  0  0  0  0  1  0  0
    ## 2            1  0  0  1  0  0  2  1  0
    ## 3            1  0  0  0  1  0  3  1  0
    ## 4            1  0  0  0  0  1  4  0  0
    ## 5            1  1  0  0  0  0  5  0  0
    ## 6            1  1  0  1  0  0  6  0  1
    ## 7            1  1  0  0  1  0  7  1  0
    ## 8            1  1  0  0  0  1  8  0  0
    ## 9            1  0  1  0  0  0  9  0  1
    ## 10           1  0  1  1  0  0 10  0  0
    ## 11           1  0  1  0  1  0 11  1  0
    ## 12           1  0  1  0  0  1 12  0  1
    ## attr(,"assign")
    ## [1] 0 1 1 2 2 2 3 4 4
    ## attr(,"contrasts")
    ## attr(,"contrasts")$a
    ## [1] "contr.treatment"
    ## 
    ## attr(,"contrasts")$b
    ## [1] "contr.treatment"
    ## 
    ## attr(,"contrasts")$d
    ## [1] "contr.treatment"

``` r
(mm1 <- model.matrix(~ . -1, data = dd)) # no intercept
```

    ##    a1 a2 a3 b2 b3 b4  c dY dZ
    ## 1   1  0  0  0  0  0  1  0  0
    ## 2   1  0  0  1  0  0  2  1  0
    ## 3   1  0  0  0  1  0  3  1  0
    ## 4   1  0  0  0  0  1  4  0  0
    ## 5   0  1  0  0  0  0  5  0  0
    ## 6   0  1  0  1  0  0  6  0  1
    ## 7   0  1  0  0  1  0  7  1  0
    ## 8   0  1  0  0  0  1  8  0  0
    ## 9   0  0  1  0  0  0  9  0  1
    ## 10  0  0  1  1  0  0 10  0  0
    ## 11  0  0  1  0  1  0 11  1  0
    ## 12  0  0  1  0  0  1 12  0  1
    ## attr(,"assign")
    ## [1] 1 1 1 2 2 2 3 4 4
    ## attr(,"contrasts")
    ## attr(,"contrasts")$a
    ## [1] "contr.treatment"
    ## 
    ## attr(,"contrasts")$b
    ## [1] "contr.treatment"
    ## 
    ## attr(,"contrasts")$d
    ## [1] "contr.treatment"

``` r
#if only applied to one column
head(mm2 <- model.matrix(~ a , data = dd)) # no intercept
```

    ##   (Intercept) a2 a3
    ## 1           1  0  0
    ## 2           1  0  0
    ## 3           1  0  0
    ## 4           1  0  0
    ## 5           1  1  0
    ## 6           1  1  0

``` r
head(mm3 <- model.matrix(~ a -1, data = dd)) # no intercept
```

    ##   a1 a2 a3
    ## 1  1  0  0
    ## 2  1  0  0
    ## 3  1  0  0
    ## 4  1  0  0
    ## 5  0  1  0
    ## 6  0  1  0

#### `Matrix::sparse.model.matrix`

``` r
sm1 <- Matrix::sparse.model.matrix(~ . -1, data = dd)
str(sm1)  #class 'dgCMatrix'
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:40] 0 1 2 3 4 5 6 7 8 9 ...
    ##   ..@ p       : int [1:10] 0 4 8 12 15 18 21 33 37 40
    ##   ..@ Dim     : int [1:2] 12 9
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:12] "1" "2" "3" "4" ...
    ##   .. ..$ : chr [1:9] "a1" "a2" "a3" "b2" ...
    ##   ..@ x       : num [1:40] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..@ factors : list()

``` r
(sm2 <- Matrix::sparse.model.matrix(~ a -1, data = dd))
```

    ## 12 x 3 sparse Matrix of class "dgCMatrix"
    ##    a1 a2 a3
    ## 1   1  .  .
    ## 2   1  .  .
    ## 3   1  .  .
    ## 4   1  .  .
    ## 5   .  1  .
    ## 6   .  1  .
    ## 7   .  1  .
    ## 8   .  1  .
    ## 9   .  .  1
    ## 10  .  .  1
    ## 11  .  .  1
    ## 12  .  .  1

#### `MatrixModels::model.Matrix`

> `model.Matrix` which is a simple wrapper around the traditional `model.matrix` and returns a *ddenseModelMatrix* object.

``` r
rm1 <- MatrixModels::model.Matrix(~ . -1, data = dd)
str(rm1)  #class 'ddenseModelMatrix'
```

    ## Formal class 'ddenseModelMatrix' [package "MatrixModels"] with 6 slots
    ##   ..@ x        : num [1:108] 1 1 1 1 0 0 0 0 0 0 ...
    ##   ..@ Dim      : int [1:2] 12 9
    ##   ..@ Dimnames :List of 2
    ##   .. ..$ : chr [1:12] "1" "2" "3" "4" ...
    ##   .. ..$ : chr [1:9] "a1" "a2" "a3" "b2" ...
    ##   ..@ factors  : list()
    ##   ..@ assign   : int [1:9] 1 1 1 2 2 2 3 4 4
    ##   ..@ contrasts:List of 3
    ##   .. ..$ a: chr "contr.treatment"
    ##   .. ..$ b: chr "contr.treatment"
    ##   .. ..$ d: chr "contr.treatment"

``` r
(rm2 <- MatrixModels::model.Matrix(~ a -1, data = dd))
```

    ## 12 x 3 Matrix of class "ddenseModelMatrix"
    ##    a1 a2 a3
    ## 1   1  0  0
    ## 2   1  0  0
    ## 3   1  0  0
    ## 4   1  0  0
    ## 5   0  1  0
    ## 6   0  1  0
    ## 7   0  1  0
    ## 8   0  1  0
    ## 9   0  0  1
    ## 10  0  0  1
    ## 11  0  0  1
    ## 12  0  0  1

### `caret::dummyVars`

``` r
library(caret)
dummy.vars <- dummyVars(~ . , data = dd)
str(dummy.vars)  #dummyVars list
```

    ## List of 9
    ##  $ call      : language dummyVars.default(formula = ~., data = dd)
    ##  $ form      :Class 'formula'  language ~.
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##  $ vars      : chr [1:4] "a" "b" "c" "d"
    ##  $ facVars   : chr [1:3] "a" "b" "d"
    ##  $ lvls      :List of 3
    ##   ..$ a: chr [1:3] "1" "2" "3"
    ##   ..$ b: chr [1:4] "1" "2" "3" "4"
    ##   ..$ d: chr [1:3] "X" "Y" "Z"
    ##  $ sep       : chr "."
    ##  $ terms     :Classes 'terms', 'formula'  language ~a + b + c + d
    ##   .. ..- attr(*, "variables")= language list(a, b, c, d)
    ##   .. ..- attr(*, "factors")= int [1:4, 1:4] 1 0 0 0 0 1 0 0 0 0 ...
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:4] "a" "b" "c" "d"
    ##   .. .. .. ..$ : chr [1:4] "a" "b" "c" "d"
    ##   .. ..- attr(*, "term.labels")= chr [1:4] "a" "b" "c" "d"
    ##   .. ..- attr(*, "order")= int [1:4] 1 1 1 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 0
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. ..- attr(*, "predvars")= language list(a, b, c, d)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:4] "factor" "factor" "numeric" "factor"
    ##   .. .. ..- attr(*, "names")= chr [1:4] "a" "b" "c" "d"
    ##  $ levelsOnly: logi FALSE
    ##  $ fullRank  : logi FALSE
    ##  - attr(*, "class")= chr "dummyVars"

``` r
dd.dummy   <- predict(dummy.vars, dd)
dd.dummy  # numeric matrix
```

    ##    a.1 a.2 a.3 b.1 b.2 b.3 b.4  c d.X d.Y d.Z
    ## 1    1   0   0   1   0   0   0  1   1   0   0
    ## 2    1   0   0   0   1   0   0  2   0   1   0
    ## 3    1   0   0   0   0   1   0  3   0   1   0
    ## 4    1   0   0   0   0   0   1  4   1   0   0
    ## 5    0   1   0   1   0   0   0  5   1   0   0
    ## 6    0   1   0   0   1   0   0  6   0   0   1
    ## 7    0   1   0   0   0   1   0  7   0   1   0
    ## 8    0   1   0   0   0   0   1  8   1   0   0
    ## 9    0   0   1   1   0   0   0  9   0   0   1
    ## 10   0   0   1   0   1   0   0 10   1   0   0
    ## 11   0   0   1   0   0   1   0 11   0   1   0
    ## 12   0   0   1   0   0   0   1 12   0   0   1

### `base::model.matrix`

``` r
mm.dummy <- cbind(model.matrix(~ a - 1, data = dd),
                  model.matrix(~ b - 1, data = dd),
                  c = dd[, "c"],
                  model.matrix(~ d - 1, data = dd)
                  )
mm.dummy
```

    ##    a1 a2 a3 b1 b2 b3 b4  c dX dY dZ
    ## 1   1  0  0  1  0  0  0  1  1  0  0
    ## 2   1  0  0  0  1  0  0  2  0  1  0
    ## 3   1  0  0  0  0  1  0  3  0  1  0
    ## 4   1  0  0  0  0  0  1  4  1  0  0
    ## 5   0  1  0  1  0  0  0  5  1  0  0
    ## 6   0  1  0  0  1  0  0  6  0  0  1
    ## 7   0  1  0  0  0  1  0  7  0  1  0
    ## 8   0  1  0  0  0  0  1  8  1  0  0
    ## 9   0  0  1  1  0  0  0  9  0  0  1
    ## 10  0  0  1  0  1  0  0 10  1  0  0
    ## 11  0  0  1  0  0  1  0 11  0  1  0
    ## 12  0  0  1  0  0  0  1 12  0  0  1
