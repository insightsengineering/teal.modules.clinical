#' Substitute in Quoted Expressions
#'
#' This version of substitute is needed because \code{substitute} does not
#' evaluate it's first argument, and it's often useful to be able to modify
#' a quoted expression.
#'
#' @md
#' @param qexpr (`language`)\cr a quoted expression.
#' @param env (`environment` or `list`)\cr requested variable substitutions.
#'
#' @return The modified expression.
#' @note This is simplified from the package `pryr` to avoid another dependency.
#' @seealso [substitute_names()]
#'
#' @examples
#' x <- quote(a + b)
#' substitute(x, list(a = 1, b = 2))
#' teal.modules.clinical:::substitute_q(x, list(a = 1, b = 2))
#' @keywords internal
substitute_q <- function(qexpr, env) {
  stopifnot(is.language(qexpr))
  call <- substitute(substitute(qexpr, env), list(qexpr = qexpr))
  eval(call)
}

#' Substitute Names in a Quoted Expression
#'
#' This function substitutes the names on both left- and right-hand sides in a quoted expression.
#' In addition it can also do other standard substitutions on the right-hand side.
#'
#' @md
#' @param expr (`language`)\cr an expression.
#' @param names (named `list` of `name`)\cr requested name substitutions.
#' @param others (named `list`)\cr requested other substitutions which will only happen on the
#'   right-hand side.
#'
#' @name substitute_names
#' @return The modified expression.
#' @seealso [substitute_q()]
#'
#' @examples
#' teal.modules.clinical:::substitute_names(
#'   mutate(a = a + b, b = c + d),
#'   names = list(a = as.name("d"), b = as.name("e"))
#' )
#' teal.modules.clinical:::substitute_names(
#'   c(a = fun(a), b = 3),
#'   names = list(a = as.name("b"), b = as.name("c"))
#' )
#' teal.modules.clinical:::substitute_names(
#'   c(a = fun(a), b = bla),
#'   names = list(a = as.name("b"), b = as.name("c")),
#'   others = list(bla = "foo")
#' )
#' @keywords internal
substitute_names <- function(expr, names, others = list()) {
  checkmate::assert_list(names, min.len = 1, names = "unique", types = "name")
  checkmate::assert_list(others, min.len = 0, names = "unique")
  checkmate::assert_names(names(names), disjunct.from = names(others))

  expr <- substitute(expr)
  expr <- substitute_rhs(expr, c(names, others))
  substitute_lhs_names(expr, names)
}

#' @md
#' @describeIn substitute_names Helper function to just substitute the top-level names on the left-hand side in a
#'   quoted expression.
#' @inheritParams substitute_q
#' @keywords internal
h_subst_lhs_names <- function(qexpr, names) {
  will_replace <- names(names)
  to_replace <- names(qexpr)
  matches <- match(x = to_replace, table = will_replace)
  which_found <- which(!is.na(matches))
  names_as_strings <- sapply(names, as.character)
  names(qexpr)[which_found] <- names_as_strings[matches[which_found]]
  qexpr
}

#' @md
#' @describeIn substitute_names recursively substitutes all names on the left-hand sides in a quoted expression.
#' @inheritParams substitute_q
#' @keywords internal
substitute_lhs_names <- function(qexpr, names) {
  if (length(qexpr) == 1L) {
    return(qexpr)
  }
  qexpr <- h_subst_lhs_names(qexpr, names)
  for (i in seq_along(qexpr)) {
    qexpr[[i]] <- substitute_lhs_names(qexpr[[i]], names)
  }
  qexpr
}

#' @md
#' @describeIn substitute_names substitutes on the right-hand side in a quoted expression.
#'   Note that this is just a synonym for [substitute_q()].
#' @inheritParams substitute_q
#' @keywords internal
substitute_rhs <- function(qexpr, env) {
  substitute_q(qexpr, env)
}
