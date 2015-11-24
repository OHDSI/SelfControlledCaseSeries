# Workaround to fix unit tests in check as described here:
# https://github.com/hadley/testthat/issues/86
Sys.setenv("R_TESTS" = "")

library(testthat)
library(SelfControlledCaseSeries)
test_check("SelfControlledCaseSeries")
