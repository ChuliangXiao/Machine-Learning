Titanic: Machine Learning from Disaster
================

### Load packages

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1.9000     v purrr   0.2.5     
    ## v tibble  1.4.2          v dplyr   0.7.5     
    ## v tidyr   0.8.1          v stringr 1.3.1     
    ## v readr   1.1.1          v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rpart)
library(rpart.plot)
```

### Load data

``` r
train_df  <- read_csv("data/train.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   PassengerId = col_integer(),
    ##   Survived = col_integer(),
    ##   Pclass = col_integer(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_integer(),
    ##   Parch = col_integer(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

``` r
test_df   <- read_csv("data/test.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   PassengerId = col_integer(),
    ##   Pclass = col_integer(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_integer(),
    ##   Parch = col_integer(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )
