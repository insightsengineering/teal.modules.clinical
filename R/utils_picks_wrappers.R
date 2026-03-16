# Internal helpers bridging the teal.picks API.

#' @keywords internal
#' Applies .qenv_merge to a teal_data object using a named list of selectors.
#' Selectors can be either reactive picks or static picks objects.
#' @param x teal_data object to merge
#' @param selectors named list of reactive or static picks
#' @param output_name name of the output dataset
#' @return teal_data with merged dataset
qenv_merge_selectors <- function(x, selectors, output_name = "ANL") {
  # Unwrap reactives to get picks objects
  resolved_selectors <- lapply(selectors, function(sel) {
    if (inherits(sel, "reactive") || is.function(sel)) {
      tryCatch(sel(), error = function(e) NULL)
    } else {
      sel
    }
  })
  # Filter out NULLs (unresolved reactives)
  resolved_selectors <- Filter(Negate(is.null), resolved_selectors)
  if (length(resolved_selectors) == 0) {
    return(x)
  }
  teal.transform:::.qenv_merge(x, selectors = resolved_selectors, output_name = output_name)
}

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


