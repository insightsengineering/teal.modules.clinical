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


#' Expression Deparsing
#'
#' Deparse an expression into a `string`.
#'
#' @param expr (`call`)\cr or an object which can be used as so.
#'
#' @export
#' @return a `string`.
#' @examples
#' expr <- quote(
#'   basic_table() %>%
#'   split_cols_by(var = "ARMCD") %>%
#'   test_proportion_diff(
#'     vars = "rsp", method = "cmh", variables = list(strata = "strat")
#'   ) %>%
#'   build_table(df = dta)
#' )
#'
#' h_concat_expr(expr)
#'
h_concat_expr <- function(expr) {
  expr <- deparse(expr)
  paste(expr, collapse = " ")
}


#' Expressions as a Pipeline
#'
#' Concatenate expressions in a single pipeline-flavor expression.
#'
#' @param ... (`call`)\cr or objects which can be used as so.
#'    (e.g. `name`).
#'
#' @export
#' @examples
#'
#' result <- pipe_expr(
#'   expr1 = substitute(df),
#'   expr2 = substitute(head)
#' )
#' result
#'
pipe_expr <- function(...) {
  exprs <- unlist(list(...))
  exprs <- lapply(exprs, h_concat_expr)
  exprs <- unlist(exprs)
  exprs <- paste(exprs, collapse = " %>% ")
  str2lang(exprs)
}


#' Styled Code Printing
#'
#' Deparse an expression and display the code following NEST conventions.
#'
#' @param expr (`call`)\cr or possibly understood as so.
#'
#' @note The package `prettycode` must be installed to turn on colored output,
#'   hence the warning.
#' @importFrom styler style_text
#' @export
#' @examples
#' expr <- quote(
#'   basic_table() %>%
#'     split_cols_by(var = "ARMCD") %>%
#'     test_proportion_diff(
#'       vars = "rsp", method = "cmh", variables = list(strata = "strat")
#'     ) %>%
#'     build_table(df = dta)
#' )
#'
#' styled_expr(expr)
#'
styled_expr <- function(expr) {
  styler::style_text(text = deparse(expr), style = teal.devel::nest_style)
}


#' Expression List
#'
#' Add a new expression to a list (of expressions).
#'
#' @param expr_ls (`list` of `call`)\cr the list to which a new expression
#'   should be added.
#' @param new_expr (`call`)\cr the new expression to add.
#'
#' @return a `list` of `call`.
#'
#' @details Offers a stricter control to add new expressions to an existing
#'   list. The list of expressions can be later used to generate a pipeline,
#'   for instance with `pipe_expr`.
#'
#' @import assertthat
#' @export
#' @examples
#'
#' lyt <- list()
#' lyt <- add_expr(lyt, substitute(basic_table()))
#' lyt <- add_expr(
#'   lyt, substitute(split_cols_by(var = arm), env = list(armcd = "ARMCD"))
#' )
#' lyt <- add_expr(
#'   lyt,
#'   substitute(
#'     test_proportion_diff(
#'       vars = "rsp", method = "cmh", variables = list(strata = "strat")
#'     )
#'   )
#' )
#' lyt <- add_expr(lyt, quote(build_table(df = dta)))
#' pipe_expr(lyt)
#'
add_expr <- function(expr_ls, new_expr) {

  assert_that(
    is.list(expr_ls),
    is.call(new_expr)
  )

  c(
    expr_ls,
    list(new_expr)
  )
}
