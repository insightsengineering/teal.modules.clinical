pkg_name <- "teal.modules.clinical"
library(pkg_name, character.only = TRUE)
testthat::test_check(pkg_name)
