
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dsSupportClient

<!-- badges: start -->
[![R-CMD-check](https://github.com/sofiasiamp/dsSupportClient/actions/workflows/github-actions.yml/badge.svg)](https://github.com/sofiasiamp/dsSupportClient/actions/workflows/github-actions.yml)
[![codecov](https://codecov.io/gh/sofiasiamp/dsSupportClient/graph/badge.svg?token=JIMQK79E0H)](https://codecov.io/gh/sofiasiamp/dsSupportClient)
<!-- badges: end -->

These R client-side functions where created to help automate the
production of descriptive statistics by running already existing
DataSHIELD functions on any number of harmonized datasets.

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
#library(dsSupportClient)
#datashield_descriptive(dsfunction = ds.class, datasources = opals, df = "D")
```
