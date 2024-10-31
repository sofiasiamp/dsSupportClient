
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dsSupportClient

<!-- badges: start -->
<!-- badges: end -->

The `dsSupportClient` package offers a set of client-side functions that
wrap around DataSHIELD functions from `dsBase`. It is designed to
streamline the production of descriptive statistics across multiple
harmonized datasets. Some functions are still in development and may be
refined based on user feedback.

## Installation

First, you need to install the “remotes” package from CRAN:

``` r
install.packages("remotes")
library(remotes)
```

And then install this Github repository

``` r
remotes::install_github("sofiasiamp/dsSupportClient")
```

## Example

This is a basic example on how to use it:

``` r
library(dsSupportClient)
ds.wrapper(ds_function = ds.class, datasources = connections, df = "D")
```
