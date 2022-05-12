
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datashieldDescriptives

<!-- badges: start -->
<!-- badges: end -->

These R server-side functions where created to help automate the
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
remotes::install_github("sofiasiamp/datashieldDescriptives")
```

## Example

This is a basic example on how to use it:

``` r
library(datashieldDescriptives)
#> Warning: replacing previous import 'dplyr::failwith' by 'plyr::failwith' when
#> loading 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::id' by 'plyr::id' when loading
#> 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::summarize' by 'plyr::summarize' when
#> loading 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::count' by 'plyr::count' when loading
#> 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::desc' by 'plyr::desc' when loading
#> 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::mutate' by 'plyr::mutate' when
#> loading 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::arrange' by 'plyr::arrange' when
#> loading 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::rename' by 'plyr::rename' when
#> loading 'datashieldDescriptives'
#> Warning: replacing previous import 'dplyr::summarise' by 'plyr::summarise' when
#> loading 'datashieldDescriptives'
#datashield_descriptive(dsfunction = ds.class, opal_connection = opals, df = "D")
```
