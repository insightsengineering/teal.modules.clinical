#' Comparison of Lists of Expressions
#'
#' This is a helper function for our tests which works on lists of expressions.
#'
#' @param x (`list` of `language`)\cr expressions list.
#' @param y (`list` of `language`)\cr expressions list with the same length as `x`.
#'
#' @return Flag whether each of the expression in `x` is identical as the corresponding one in `y`.
#'  Matching is done via position, not name.
#'
#' @note We coded this function since sometimes the classic [testthat::expect_equal()] seems to not work
#'  as expected when there are indentation differences and thus differences between the parsed
#'  expressions. It seems difficult to reproduce this from scratch though.
#'
compare_expr_list <- function(x, y) {
  assert_that(
    is.list(x),
    is.list(y),
    identical(length(x), length(y))
  )
  for (i in seq_along(x)) {
    ok <- is.language(x[[i]]) &&
      is.language(y[[i]]) &&
      (x[[i]] == y[[i]])
    if (! ok) {
      return(FALSE)
    }
  }
  TRUE
}

expect_equal_expr_list <- function(object, expected) {
  are_equal_expr_lists <- compare_expr_list(object, expected)  #nolint
  expect(
    are_equal_expr_lists,
    failure_message = "object and expected are not equal expression lists"
  )
}
