#' Returns testing depth set by session option or by environmental variable.
#'
#' @details Looks for the session option `TESTING_DEPTH` first.
#' If not set, takes the system environmental variable `TESTING_DEPTH`.
#' If neither is set, then returns 3 by default.
#' If the value of `TESTING_DEPTH` is not a numeric of length 1, then returns 3.
#'
#' @return `numeric(1)` the testing depth.
#'
get_testing_depth <- function() {
  default_depth <- 3
  depth <- getOption("TESTING_DEPTH", Sys.getenv("TESTING_DEPTH", default_depth))
  depth <- tryCatch(
    as.numeric(depth),
    error = function(error) default_depth,
    warning = function(warning) default_depth
  )
  if (length(depth) != 1) depth <- default_depth
  depth
}

#' Skipping tests in the testthat pipeline under specific scope
#' @description This function should be used per each `testthat::test_that` call.
#'   Each of the call should specify an appropriate depth value.
#'   The depth value will set the appropriate scope so more/less time consuming tests could be recognized.
#'   The environment variable `TESTING_DEPTH` is used for changing the scope of `testthat` pipeline.
#' `TESTING_DEPTH` interpretation for each possible value:
#' \itemize{
#' \item{0}{no tests at all}
#' \item{1}{fast - small scope - executed on every commit}
#' \item{3}{medium - medium scope - daily integration pipeline}
#' \item{5}{slow - all tests - daily package tests}
#' }
#' @param depth `numeric` the depth of the testing evaluation,
#'   has opposite interpretation to environment variable `TESTING_DEPTH`.
#'   So e.g. `0` means run it always and `5` means a heavy test which should be run rarely.
#'   If the `depth` argument is larger than `TESTING_DEPTH` then the test is skipped.
#' @importFrom testthat skip
#' @return `NULL` or invoke an error produced by `testthat::skip`
#' @note By default `TESTING_DEPTH` is equal to 3 if there is no environment variable for it.
#' By default `depth` argument lower or equal to 3 will not be skipped because by default `TESTING_DEPTH`
#' is equal to 3. To skip <= 3 depth tests then the environment variable has to be lower than 3 respectively.
skip_if_too_deep <- function(depth) { # nolintr
  checkmate::assert_numeric(depth, len = 1, lower = 0, upper = 5)
  testing_depth <- get_testing_depth() # by default 3 if there are no env variable
  if (testing_depth < depth) {
    testthat::skip(paste("testing depth", testing_depth, "is below current testing specification", depth))
  }
}

#' Returns a character vector of the files that are affected by the commit.
#'
#' @details Looks for the session option `AFFECTED_FILES`.
#' If unset, then returns all the .R files in the R directory.
#' When any R file other than `R/tm_xxxx.R` is modified, then it will return all the .R files in the R directory
#'
#' @return `character` the name of files that are modified.
#'
get_affected_files <- function() {
  r_path <- testthat::test_path("..", "..", "R")
  r_files <- list.files(r_path, pattern = "\\.R$", full.names = FALSE)
  module_files <- grep("^tm_", r_files, value = TRUE)
  non_module_files <- r_files[!r_files %in% module_files]
  affected_files <- getOption("AFFECTED_FILES", r_files)

  # When non-module files are modified, it has a potential to modify the modules. Treating it as changing all R files.
  if (any(affected_files %in% non_module_files)) {
    affected_files <- r_files
  }
  affected_files
}

#' Skipping e2e-tests in the testthat pipeline when the related module file is not modified.
#' @description This function should be used per each `testthat::test_that` call.
#'   Each of the call should specify the module file name should change to trigger the testthat scope.
#'   The affected files are set using the system option `AFFECTED_FILES`.
#' @param file `character` the file name which should affect the test.
#' @return `NULL` or invoke an error produced by `testthat::skip`
#' @note By default `AFFECTED_FILES` lists all the R files, so it will not skip any tests unless explicity specified.
#' Additionally, any change made to the non-module files will make the affected files to be all files.
ship_if_file_not_modified <- function(file) {
  checkmate::assert_character(file)
  affected_files <- get_affected_files()
  if (!file %in% affected_files) {
    testthat::skip(sprintf("The file `%s` is not modified in the commit. So, skipping the test.", file))
  }
}
