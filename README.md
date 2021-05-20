
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eqt

## Overview

The **eqt** package contains a collection of tools commonly used to
perform **small-sample equating**. The following transformations are
possible:

-   Identity
-   Mean
-   Linear
-   Circle-arc

If anchor items are included, then any of the following methods may be
applied:

-   Nominal weights
-   Tucker
-   Levine observed-score
-   Levine true-score
-   Chained

## Installation

Install the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("kyliegorney/eqt")
```

## Example: Random Groups Design

In the following examples, consider a 100-item test that is taken by 25
examinees. Use the data stored in the object `random_groups_dat`, and
let `x` and `y` be the item responses from the new and old forms,
respectively.

``` r
library(eqt)

# Extract data
list2env(random_groups_dat, envir = globalenv())
#> <environment: R_GlobalEnv>

# Identity equating
(id <- eqt(x, y))
#> intercept     slope 
#>         0         1

# Mean equating
(me <- eqt(x, y, linear = "mean"))
#> intercept     slope 
#>      -1.8       1.0

# Linear equating
(li <- eqt(x, y, linear = "linear"))
#> intercept     slope 
#>     -0.87      0.98
```

Individual equatings may be combined to form a weighted result using
`multipleqt()`.

``` r
(id_li <- multipleqt(id, li))
#>           eqt1  eqt2   eqt
#> intercept    0 -0.87 -0.44
#> slope        1  0.98  0.99
```

Use `boot()` to compute the bootstrap standard errors.

``` r
# Use an eqt object
set.seed(1004)
boot(li)
#> Mean SEE: 6.238598

# Use a multipleqt object
set.seed(1004)
boot(id_li)
#> Mean SEE: 3.119299
```

## Example: Common-Item Nonequivalent Groups Design

In this example, suppose the two test forms have the first 30 items in
common. Use the data stored in the object `nonequiv_groups_dat`, and let
`x` and `y` be the item responses from the new and old forms,
respectively.

``` r
# Extract data
list2env(nonequiv_groups_dat, envir = globalenv())
#> <environment: R_GlobalEnv>

# Tucker mean equating
eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker")
#> intercept     slope 
#>     -0.57      1.00

# Levine observed-score equating with an external anchor
eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "levine observed",
    anchor = "external")
#> intercept     slope 
#>     -1.21      1.00

# Simplified circle-arc with chained linear equating
eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained",
    circle = "simplified")
#> intercept     slope  x_center  y_center    radius 
#>      0.00      1.00     50.00   1381.96   1382.86
```
