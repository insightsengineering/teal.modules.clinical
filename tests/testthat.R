library(testthat)

test_results <- test_check("teal.modules.clinical")
saveRDS(test_results, "unit_testing_results.rds")
