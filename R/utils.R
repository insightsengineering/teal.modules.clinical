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
#' @param exprs (`list` of `call`)\cr expressions to concatenate in a
#'   pipeline (`%>%`).
#' @param pipe_str (`string`)\cr the character which separates the expressions.
#'
#' @export
#' @examples
#'
#' result <- pipe_expr(
#'   list(
#'     expr1 = substitute(df),
#'     expr2 = substitute(head)
#'   )
#' )
#' result
#'
pipe_expr <- function(exprs, pipe_str = "%>%") {
  exprs <- lapply(exprs, h_concat_expr)
  exprs <- unlist(exprs)
  exprs <- paste(exprs, collapse = pipe_str)
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
    is.call(new_expr) || is.name(new_expr)
  )

  # support nested expressions such as expr({a <- 1; b <- 2})
  if (is(new_expr, "{")) {
    res <- expr_ls
    for (idx in seq_along(new_expr)[-1]) {
      res <- add_expr(res, new_expr[[idx]])
    }
    return(res)
  }

  c(
    expr_ls,
    list(new_expr)
  )
}


#' Expressions in Brackets
#'
#' Groups several expressions in a single _bracketed_ expression.
#'
#' @param exprs (`list` of `call`)\cr expressions to concatenate into
#'   a single _bracketed_ expression.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#' adsl <- radsl(cached = TRUE)
#' adrs <- radrs(cached = TRUE)
#'
#' expr1 <- substitute(
#'   expr = anl <- subset(df, PARAMCD == param),
#'   env = list(df = as.name("adrs"), param = "INVET")
#' )
#' expr2 <- substitute(expr = anl$rsp_lab <- d_onco_rsp_label(anl$AVALC))
#' expr3 <- substitute(
#'   expr = {
#'     anl$is_rsp <- anl$rsp_lab %in%
#'       c("Complete Response (CR)", "Partial Response (PR)")
#'   }
#' )
#'
#' res <- bracket_expr(list(expr1, expr2, expr3))
#' eval(res)
#' table(anl$rsp_lab, anl$is_rsp)
#'
bracket_expr <- function(exprs) {

  expr <- lapply(exprs, deparse)

  # Because `deparse` returns a vector accounting for line break attempted
  # for string longer than max `width.cutoff = 500`.
  expr <- lapply(expr, paste, collapse = " ")

  expr <- paste(
    c(
      "{",
      unlist(expr),
      "}"
    ),
    collapse = "\n"
  )
  expr <- parse(text = expr)
  expr <- as.call(expr)[[1]]
  attributes(expr) <- NULL
  expr
}

#' Convert choices_selected to select_spec
#'
#' @param cs (\code{choices_selected}) object to be transformed
#' @param multiple (\code{logical}) whether allow multiple selection in the select input
#'
#' @return (\code{selecte_spec})
cs_to_select_spec <- function(cs, multiple = FALSE) {
  stopifnot(is.choices_selected(cs))
  stopifnot(is_logical_single(multiple))

  select_spec(
    choices = cs$choices,
    selected = cs$selected,
    fixed = cs$fixed,
    multiple = multiple
  )
}

#' Convert choices_selected to filter_spec
#'
#' @inheritParams cs_to_select_spec
#'
#' @return (\code{filter_spec})
cs_to_filter_spec <- function(cs, multiple = FALSE) {
  stopifnot(is.choices_selected(cs))
  stopifnot(is_logical_single(multiple))

  vars <- if (inherits(cs, "delayed_choices_selected")) {
    cs$choices$var_choices
  } else {
    attr(cs$choices, "var_choices")
  }

  filter_spec(
    vars = vars,
    choices = cs$choices,
    selected = cs$selected,
    multiple = multiple,
    drop_keys = FALSE
  )
}

#' Convert choices_selected to data_extract_spec with only select_spec
#'
#' @inheritParams cs_to_select_spec
#' @param dataname (\code{character}) name of the data
#'
#' @return (\code{data_extract_spec})
cs_to_des_select <- function(cs, dataname, multiple = FALSE) {
  cs_sub <- substitute(cs)
  cs_name <- if (is.symbol(cs_sub)) as.character(cs_sub) else "cs"

  stop_if_not(
    list(
      is.cs_or_des(cs),
      paste(cs_name, "must be a choices selected object or a data extract spec")
    ),
    is_character_single(dataname),
    is_logical_single(multiple)
  )
  if (!multiple) {
    stop_if_not(
      list(
        length(cs$selected) == 1 || is.null(cs$selected),
        paste(cs_name, "must only have 1 selected value")
      )
    )
  }

  if (is.choices_selected(cs)) {
    data_extract_spec(
      dataname = dataname,
      select = cs_to_select_spec(cs, multiple = multiple)
    )
  } else {
    return(cs)
  }
}

#' Convert choices_selected to data_extract_spec with only filter_spec
#'
#' @inheritParams cs_to_des_select
#'
#' @return (\code{data_extract_spec})
cs_to_des_filter <- function(cs, dataname, multiple = FALSE) {
  cs_sub <- substitute(cs)
  cs_name <- if (is.symbol(cs_sub)) as.character(cs_sub) else "cs"

  stop_if_not(
    list(
      is.cs_or_des(cs),
      paste(cs_name, "must be a choices selected object or a data extract spec")
    ),
    is_character_single(dataname),
    is_logical_single(multiple)
  )
  if (!multiple) {
    stop_if_not(
      list(
        length(cs$selected) == 1 || is.null(cs$selected),
        paste(cs_name, "must only have 1 selected value")
      )
    )
  }

  if (is.choices_selected(cs)) {
    data_extract_spec(
      dataname = dataname,
      filter = cs_to_filter_spec(cs, multiple = multiple)
    )
  } else {
    return(cs)
  }
}

#' Whether object is of class \code{choices_selected} or \code{data_extract_spec}
#'
#' @param x object to be checked
#'
#' @return (\code{logical})
is.cs_or_des <- function(x) { # nolint
  is.choices_selected(x) || is(x, "data_extract_spec")
}

#' Split-Column Expression
#'
#' Renders the expression for column split in `rtables` depending on:
#' - the expected or not arm comparison
#' - the expected or not arm combination
#'
#' @param compare (`flag`)\cr if `TRUE` the reference level is included.
#' @param combine (`flag`)\cr if `TRUE` the group combination is included.
#' @param ref (`string`)\cr the reference level (not used for `combine = TRUE`).
#' @param arm_var (`string`)\cr the arm or grouping variable name.
#'
split_col_expr <- function(compare, combine, ref, arm_var) {

  if  (compare & combine) {
    substitute(
      expr = split_cols_by_groups(
        var = arm_var,
        groups_list = groups,
        ref_group = names(groups)[1]
      ),
      env = list(
        arm_var = arm_var
      )
    )
  } else if (compare & !combine) {
    substitute(
      expr = split_cols_by(
        var = arm_var,
        ref_group = ref
      ),
      env = list(
        arm_var = arm_var,
        ref = ref
      )
    )
  } else if (!compare) {
    substitute(
      expr = split_cols_by(var = arm_var),
      env = list(arm_var = arm_var)
    )
  }
}

#' Split `choices_selected` objects with interactions into
#' their component variables
#' @md
#'
#' @param x (`choices_selected`) object with interaction terms
#'
#' @note uses the regex `\\*|:` to perform the split.
split_choices <- function(x) {
  stopifnot(is.choices_selected(x))
  stopifnot(is_character_vector(x$choices))

  split_x <- x
  split_x$choices <- split_interactions(x$choices)
  if (!is.null(x$selected)) {
    split_x$selected <- split_interactions(x$selected)
  }

  return(split_x)
}

#' Get input id for a data extract input
#'
#' @param varname (`character`) name of the variable corresponding to the
#'   data extract input.
#' @param dataname (`character`) name of the dataset corresponding to the
#'   data extract input.
#' @param filter (`logical`) optional; string output will end with "-select" suffix if FALSE
#'   and "-filter1" if TRUE
extract_input <- function(varname, dataname, filter = FALSE) {
  if (filter) {
    paste0(varname, "-dataset_", dataname, "_singleextract-filter1")
  } else {
    paste0(varname, "-dataset_", dataname, "_singleextract-select")
  }
}

#' Split interaction terms into their component variables
#'
#' @param x (`character`) string representing the interaction
#'  usually in the form `x:y` or `x*y`.
#' @param by (`character`) regex with which to split the interaction
#'  term by.
#'
split_interactions <- function(x, by = "\\*|:") {
  if (length(x) >= 1) {
    unique(unlist(strsplit(x, split = by)))
  } else {
    NULL
  }
}


#' Expression: Arm Preparation
#'
#' The function generate the standard expression for pre-processing of dataset
#' in teal module applications. This is especially of interest when the same
#' preprocessing steps needs to be applied similarly to several datasets
#' (e.g. ADSL and ADRS).
#'
#' @details
#' In `teal.modules.clinical`, the user interface includes manipulation of
#' the study arms. Classically: the arm variable itself (e.g. `ARM`, `ACTARM`),
#' the reference arm (0 or more), the comparison arm (1 or more) and the
#' possibility to combine comparison arms.
#'
#' Note that when no arms should be compared with each other, then the produced
#' expression is reduced to optionally dropping non-represented levels of the arm.
#'
#' When comparing arms, the pre-processing includes three steps:
#' 1. Filtering of the dataset to retain only the arms of interest (reference
#' and comparison).
#' 2. Optional, if more than one arm is designated as _reference_ they are
#' combined into a single level.
#' 3. The reference is explicitly reassigned and the non-represented levels of
#' arm are dropped.
#'
#' @inheritParams argument_convention
#' @param ref_arm_val (`string`)\cr replacement name for the reference level.
#' @param drop (`flag`)\cr drop the unused variable levels.
#' @examples
#'
#' \dontrun{
#' teal.modules.clinical::prepare_arm(
#'   dataname = "adrs",
#'   arm_var = "ARMCD",
#'   ref_arm = "ARM A",
#'   comp_arm = c("ARM B", "ARM C")
#' )
#'
#' teal.modules.clinical::prepare_arm(
#'   dataname = "adsl",
#'   arm_var = "ARMCD",
#'   ref_arm = c("ARM B", "ARM C"),
#'   comp_arm = "ARM A"
#' )
#' }
#'
prepare_arm <- function(dataname,
                        arm_var,
                        ref_arm,
                        comp_arm,
                        compare_arm = !is.null(ref_arm),
                        ref_arm_val = paste(ref_arm, collapse = "/"),
                        drop = TRUE) {
  assert_that(
    is.string(dataname),
    is.string(arm_var),
    is.null(ref_arm) || is.character(ref_arm),
    is.character(comp_arm) || is.null(comp_arm),
    is.flag(compare_arm),
    is.string(ref_arm_val),
    is.flag(drop)
  )

  data_list <- list()

  if (compare_arm) {
    # Data are filtered to keep only arms of interest.
    data_list <- add_expr(
      data_list,
      substitute(
        expr = dataname %>%
          filter(arm_var %in% arm_val),
        env = list(
          dataname = as.name(dataname),
          arm_var = as.name(arm_var),
          arm_val = if (compare_arm) c(ref_arm, comp_arm) else comp_arm
        )
      )
    )

    # Several reference levels are combined.
    if (length(ref_arm) > 1) {
      data_list <- add_expr(
        data_list,
        substitute_names(
          expr = mutate(arm_var = combine_levels(arm_var, levels = ref_arm, new_level = ref_arm_val)),
          names = list(arm_var = as.name(arm_var)),
          others = list(ref_arm = ref_arm, ref_arm_val = ref_arm_val)
        )
      )
    }

    # Reference level is explicit.
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = mutate(arm_var = relevel(arm_var, ref = ref_arm_val)),
        names = list(arm_var = as.name(arm_var)),
        others = list(ref_arm_val = ref_arm_val)
      )
    )
  }  else {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = dataname,
        env = list(dataname = as.name(dataname))
      )
    )
  }

  # Unused levels are optionally dropped.
  if (drop) {
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = mutate(arm_var = droplevels(arm_var)),
        names = list(arm_var = as.name(arm_var))
      )
    )
  }

  pipe_expr(data_list)
}
