#' Dynamic assertion
#'
#' Verifies assertions for dynamic outputs.
#'
#' @name dyn_assertion
#' @keywords internal
NULL

#' @describeIn dyn_assertion the dataset is large enough.
#'
#' @param data (`dataframe`).
#' @param min_nrow (`number`)\cr minimum number of rows for a valid analysis.
#'
#' @export
#'
#' @examples
#'
#' library(assertthat)
#' library(testthat)
#' expect_error(assertthat::assert_that(teal_enough_rows(data = iris, min_nrow = 1500)))
teal_enough_rows <- function(data, min_nrow) nrow(data) >= min_nrow
assertthat::on_failure(teal_enough_rows) <- function(call, env) {
  call[[1]] <- validate_enough_rows
  eval(call, envir = env)
}
validate_enough_rows <- function(data, min_nrow) {
  shiny::validate(
    shiny::need(
      FALSE,
      label = paste0(
        substitute(data),
        ": Minimum number of records not met: >= ", min_nrow,
        " records required."
      )
    )
  )
}

#' @describeIn dyn_assertion the element exist.
#' @param str (`name`)\cr the name of the object which must _exist_.
#' @param ... (`character`)\cr the label to be displayed.
#'
#' @export
#'
#' @examples
#'
#' armcd <- NULL
#' expect_error(assertthat::assert_that(teal_has_element(str = armcd, "ARMCD")))
teal_has_element <- function(str, ...) length(str) > 0
assertthat::on_failure(teal_has_element) <- function(call, env) {
  call[[1]] <- validate_has_elements
  eval(call, envir = env)
}

validate_has_elements <- function(str, label = str) {
  shiny::validate(
    shiny::need(
      FALSE,
      message = paste0(label, ": required variable not assigned.")
    )
  )
}
