
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nscore

<!-- badges: start -->
<!-- badges: end -->

The goal of nscore is to …

## Installation

You can install the development version of nscore like so:

``` r
remotes::install_github("probaverse/nscore")
```

## Example

Consider a numeric vector:

``` r
library(nscore)
x <- c(0.3, 0.56, NA, 0.1, -12)
```

Uniform scores:

``` r
uscore(x)
#> [1] 0.625 0.875    NA 0.375 0.125
```

Normal scores:

``` r
nscore(x)
#> [1]  0.3186394  1.1503494         NA -0.3186394 -1.1503494
```

Empirical return periods:

``` r
rpscore(x)
#> [1] 2.666667 8.000000       NA 1.600000 1.142857
```
