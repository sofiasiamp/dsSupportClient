# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

#### Necessary Installations for DSLite to work locally
#### In order to check for datashieldDescriptives to work with multiple dsBase versions, adaptation might be necessary
install.packages('dsBaseClient', repos= 'http://cran.datashield.org', dependencies=TRUE)
install.packages('dsBase', repos= 'http://cran.datashield.org', dependencies=TRUE)
install.packages('DSLite', repos = "http://cran.us.r-project.org")
install.packages('DSI', repos = "http://cran.us.r-project.org")


library(testthat)
library(dsBaseClient)
library(dsBase)
library(DSLite)
library(DSI)
library(datashieldDescriptives)

test_check("datashieldDescriptives")
