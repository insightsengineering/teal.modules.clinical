# This file contains functions that help with plotting in other modules

#' Facetting formula `x_facet ~ y_facet`
#'
#' @description `r lifecycle::badge("stable")`
#' Replaces `x_facet` or `y_facet` by . when empty character
#'
#' @md
#' @param x_facet (`character(1)`)\cr
#'  name of x facet, if empty, will not facet along x.
#' @param y_facet (`character(1)`)\cr
#'  name of y facet, if empty, will not facet along y.
#'
#' @return facet grid formula `formula(x_facet ~ y_facet)`
#'
#' @keywords internal
#'
#' @examples
#' teal.modules.clinical:::facet_grid_formula("x", "y")
facet_grid_formula <- function(x_facet, y_facet) {
  if (rlang::is_empty(x_facet)) x_facet <- "."
  if (rlang::is_empty(y_facet)) y_facet <- "."
  checkmate::assert_string(x_facet)
  checkmate::assert_string(y_facet)
  stopifnot(x_facet != y_facet)
  stats::as.formula(paste0(y_facet, " ~ ", x_facet)) # must invert it
}
