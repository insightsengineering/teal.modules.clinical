#' Shared Parameters
#'
#' @description Contains arguments that are shared between multiple functions
#'   in the package to avoid repetition using \code{inheritParams}.
#'
#' @param plot_height optional, (\code{numeric}) a vector of length three with \code{c(value, min, max)}. Specifies
#'   the height of the main plot.
#' @param plot_width optional, (\code{numeric}) a vector of length three with \code{c(value, min, max)}. Specifies
#'   the width of the main plot and renders a slider on the plot to interactively adjust the plot width.
#' @param label (\code{character}) menu item label of the module in the teal app
#'
#' @name shared_params
NULL

#' Concatenate expressions via a binary operator
#'
#' e.g. combine with \code{+} for ggplot without introducing parentheses due to associativity
#'
#' @param args arguments to concatenate with operator
#' @param bin_op binary operator to concatenate it with
#'
#' @examples
#' \dontrun{
#' # What we want to achieve
#' call("+", quote(f), quote(g))
#' call("+", quote(f), call("+", quote(g), quote(h))) # parentheses not wanted
#' call("+", call("+", quote(f), quote(g)), quote(h)) # as expected without unnecessary parentheses
#' Reduce(function(existing, new) call("+", existing, new), list(quote(f), quote(g), quote(h)))
#'
#' # how we do it
#' call_concatenate(list(quote(f), quote(g), quote(h)))
#' call_concatenate(list(quote(f)))
#' call_concatenate(list())
#' call_concatenate(list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg)))))
#'
#' eval(call_concatenate(list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg))))))
#' }
call_concatenate <- function(args, bin_op = "+") {
  stopifnot(
    is_character_single(bin_op),
    all(vapply(args, is.language, logical(1)))
  )
  # can be used for dplyr and ggplot2 to concatenate calls with +
  Reduce(function(existing, new) call(bin_op, existing, new), args)
}

# needs columns like n_, n_ARM etc. to get count from
add_count_str_to_column <- function(chunk, column, n_column = NULL) {
  n_column <- if_null(n_column, get_n_name(groupby_vars = column))
  stopifnot(
    is_character_single(column)
  )

  chunk$push(bquote({
    counts <- counts %>% mutate(
      .(as.symbol(column)) := paste0(.(as.symbol(column)), " (n = ", .(as.symbol(n_column)), ")")
    )
  }))
}

add_plot_title <- function(chunk, groupby_vars) {
  chunk$push(bquote({
    total_n <- nrow(ANL) # get it from original dataset
    plot_title <- paste0(
      "Number of patients (total N = ",
      total_n,
      .(paste0(") for each combination of (", paste(groupby_vars, collapse = ", "), ")"))
    )
    plot <- plot + ggtitle(plot_title)
  }))
}
