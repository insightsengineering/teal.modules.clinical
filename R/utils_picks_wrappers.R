# Internal helpers bridging the teal.picks API.

#' @keywords internal
#' Extracts selected variables from a named list of selectors (reactive or static picks).
#' Returns a list where each element has $variables, $datasets, and $values slots.
#' @param selectors named list of reactive or static picks
#' @return named list with resolved picks
map_merged <- function(selectors) {
  lapply(selectors, function(sel) {
    resolved <- if (inherits(sel, "reactive") || is.function(sel)) {
      tryCatch(sel(), error = function(e) NULL)
    } else {
      sel
    }
    if (is.null(resolved)) {
      list(variables = NULL, datasets = NULL, values = NULL)
    } else {
      list(
        variables = resolved$variables$selected,
        datasets = resolved$datasets$selected,
        values = if (!is.null(resolved$values)) resolved$values$selected else NULL
      )
    }
  })
}


