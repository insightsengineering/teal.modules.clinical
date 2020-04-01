library(testthat)
library(random.cdisc.data)

test_results <- test_check("teal.modules.clinical")
saveRDS(test_results, "unit_testing_results.rds")
