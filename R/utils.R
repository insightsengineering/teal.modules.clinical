#' Concatenate expressions via a binary operator
#'
#' e.g. combine with `+` for `ggplot` without introducing parentheses due to associativity
#'
#' @param args arguments to concatenate with operator
#' @param bin_op binary operator to concatenate it with
#'
#' @export
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
#' call_concatenate(
#'   list(quote(ggplot2::ggplot(mtcars)), quote(ggplot2::geom_point(ggplot2::aes(wt, mpg))))
#' )
#'
#' eval(
#'   call_concatenate(
#'     list(quote(ggplot2::ggplot(mtcars)), quote(ggplot2::geom_point(ggplot2::aes(wt, mpg))))
#'   )
#' )
#' }
call_concatenate <- function(args, bin_op = "+") {
  checkmate::assert_string(bin_op)
  checkmate::assert_list(args, types = c("symbol", "name", "call", "expression"))

  # can be used for dplyr and ggplot2 to concatenate calls with +
  Reduce(function(existing, new) call(bin_op, existing, new), args)
}

# needs columns like n_, n_ARM etc. to get count from
count_str_to_column_expr <- function(column, n_column = get_n_name(groupby_vars = column)) {
  checkmate::assert_string(column)

  substitute_names(
    expr = counts <- counts %>% dplyr::mutate(
      column_name = paste0(column_name, " (n = ", n_column_name, ")")
    ),
    names = list(column_name = as.symbol(column), n_column_name = as.symbol(n_column))
  )
}

#' Get variable labels
#'
#' @description `r lifecycle::badge("deprecated")`
#' @param datasets (`teal::FilteredData`) Data built up by teal
#' @param dataname (`character`) name of the dataset
#' @param vars (`character`) Column names in the data
#'
#' @return  `character` variable labels.
#'
#' @export
get_var_labels <- function(datasets, dataname, vars) {
  lifecycle::deprecate_warn(
    when = "0.8.14",
    what = "get_var_labels()",
    with = "formatters::var_labels()",
    details = "teal.modules.clinical won't export any utility functions except those which
      are necessary to prepare shiny app."
  )
  labels <- datasets$get_varlabels(dataname, vars)
  labels <- vapply(vars, function(x) ifelse(is.na(labels[[x]]), x, labels[[x]]), character(1))
  return(labels)
}

#' Expression Deparsing
#'
#' Deparse an expression into a `string`.
#'
#' @param expr (`call`)\cr or an object which can be used as so.
#'
#' @return a `string`.
#'
#' @export
#' @examples
#' expr <- quote(
#'   rtables::basic_table() %>%
#'     rtables::split_cols_by(var = "ARMCD") %>%
#'     test_proportion_diff(
#'       vars = "rsp", method = "cmh", variables = list(strata = "strat")
#'     ) %>%
#'     rtables::build_table(df = dta)
#' )
#'
#' teal.modules.clinical:::h_concat_expr(expr)
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
#' @param pipe_str (`character`)\cr the character which separates the expressions.
#'
#' @export
#'
#' @examples
#'
#' result <- teal.modules.clinical:::pipe_expr(
#'   list(
#'     expr1 = substitute(df),
#'     expr2 = substitute(head)
#'   )
#' )
#' result
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
#'
#' @export
#'
#' @examples
#' expr <- quote(
#'   rtables::basic_table() %>%
#'     rtables::split_cols_by(var = "ARMCD") %>%
#'     test_proportion_diff(
#'       vars = "rsp", method = "cmh", variables = list(strata = "strat")
#'     ) %>%
#'     rtables::build_table(df = dta)
#' )
#'
#' teal.modules.clinical:::styled_expr(expr)
styled_expr <- function(expr) { # nolint
  styler::style_text(text = deparse(expr))
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
#'
#' @examples
#'
#' lyt <- list()
#' lyt <- teal.modules.clinical:::add_expr(lyt, substitute(rtables::basic_table()))
#' lyt <- teal.modules.clinical:::add_expr(
#'   lyt, substitute(rtables::split_cols_by(var = arm), env = list(armcd = "ARMCD"))
#' )
#' lyt <- teal.modules.clinical:::add_expr(
#'   lyt,
#'   substitute(
#'     test_proportion_diff(
#'       vars = "rsp", method = "cmh", variables = list(strata = "strat")
#'     )
#'   )
#' )
#' lyt <- teal.modules.clinical:::add_expr(lyt, quote(rtables::build_table(df = dta)))
#' teal.modules.clinical:::pipe_expr(lyt)
add_expr <- function(expr_ls, new_expr) {
  assertthat::assert_that(
    is.list(expr_ls),
    is.call(new_expr) || is.name(new_expr)
  )

  # support nested expressions such as expr({a <- 1; b <- 2})
  if (inherits(new_expr, "{")) {
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
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adrs <- tmc_ex_adrs
#'
#' expr1 <- substitute(
#'   expr = anl <- subset(df, PARAMCD == param),
#'   env = list(df = as.name("adrs"), param = "INVET")
#' )
#' expr2 <- substitute(expr = anl$rsp_lab <- tern::d_onco_rsp_label(anl$AVALC))
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
bracket_expr <- function(exprs) {
  expr <- lapply(exprs, deparse)

  # Because `deparse` returns a vector accounting for line break attempted
  # for string longer than max `width.cutoff = 500`.
  expr <- lapply(expr, paste, collapse = "\n")

  expr <- paste(
    c(
      "{",
      unlist(expr),
      "}"
    ),
    collapse = "\n"
  )
  expr <- parse(text = expr, keep.source = FALSE)
  expr <- as.call(expr)[[1]]
  attributes(expr) <- NULL
  expr
}

#' Convert choices_selected to select_spec
#'
#' @param cs (`choices_selected`) object to be transformed. See [teal.transform::choices_selected()] for details.
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny \code{\link[shiny]{selectInput}}.
#' @param ordered (`logical(1)`) Flags whether selection order should be tracked.
#' @param label (`character`) Label to print over the selection field. For no label, set to `NULL`.
#' @export
#' @return (`select_spec`)
cs_to_select_spec <- function(cs, multiple = FALSE, ordered = FALSE, label = "Select") {
  checkmate::assert_class(cs, "choices_selected")
  checkmate::assert_flag(multiple)
  checkmate::assert_flag(ordered)

  teal.transform::select_spec(
    choices = cs$choices,
    selected = cs$selected,
    fixed = cs$fixed,
    multiple = multiple,
    ordered = ordered,
    label = label
  )
}

#' Convert choices_selected to filter_spec
#'
#' @inheritParams cs_to_select_spec
#'
#' @export
#' @return ([teal.transform::filter_spec()])
cs_to_filter_spec <- function(cs, multiple = FALSE, label = "Filter by") {
  checkmate::assert_class(cs, "choices_selected")
  checkmate::assert_flag(multiple)

  vars <- if (inherits(cs, "delayed_choices_selected")) {
    cs$choices$var_choices
  } else {
    attr(cs$choices, "var_choices")
  }

  teal.transform::filter_spec(
    vars = vars,
    choices = cs$choices,
    selected = cs$selected,
    multiple = multiple,
    drop_keys = FALSE,
    label = label
  )
}

#' Convert choices_selected to data_extract_spec with only select_spec
#'
#' @inheritParams cs_to_select_spec
#' @param dataname (`character`) name of the data
#'
#' @export
#' @return ([teal.transform::data_extract_spec()])
cs_to_des_select <- function(cs, dataname, multiple = FALSE, ordered = FALSE, label = "Select") {
  cs_sub <- substitute(cs)
  cs_name <- if (is.symbol(cs_sub)) as.character(cs_sub) else "cs"

  checkmate::assert_string(dataname)
  checkmate::assert_flag(multiple)
  checkmate::assert(
    checkmate::check_class(cs, classes = "data_extract_spec"),
    checkmate::check_class(cs, classes = "choices_selected"),
    .var.name = cs_name
  )
  if (!multiple && length(cs$selected) != 1 && !is.null(cs$selected)) {
    stop(cs_name, "must only have 1 selected value")
  }

  if (inherits(cs, "choices_selected")) {
    teal.transform::data_extract_spec(
      dataname = dataname,
      select = cs_to_select_spec(cs, multiple = multiple, ordered = ordered, label = label)
    )
  } else {
    return(cs)
  }
}

#' Convert choices_selected to data_extract_spec with only filter_spec
#'
#' @inheritParams cs_to_des_select
#' @param include_vars (`flag`)\cr whether to include the filter variables as fixed selection
#'   in the result. This can be useful for preserving for reuse in `rtables` code e.g.
#'
#' @export
#' @return ([teal.transform::data_extract_spec()])
cs_to_des_filter <- function(cs, dataname, multiple = FALSE, include_vars = FALSE, label = "Filter by") {
  cs_sub <- substitute(cs)
  cs_name <- if (is.symbol(cs_sub)) as.character(cs_sub) else "cs"

  checkmate::assert_string(dataname)
  checkmate::assert_flag(multiple)
  checkmate::assert(
    checkmate::check_class(cs, classes = "data_extract_spec"),
    checkmate::check_class(cs, classes = "choices_selected"),
    .var.name = cs_name
  )
  if (!multiple && length(cs$selected) != 1 && !is.null(cs$selected)) {
    stop(cs_name, "must only have 1 selected value")
  }

  if (inherits(cs, "choices_selected")) {
    vars <- if (inherits(cs, "delayed_choices_selected")) {
      cs$choices$var_choices
    } else {
      attr(cs$choices, "var_choices")
    }
    select <- if (include_vars) {
      teal.transform::select_spec(
        choices = vars,
        selected = vars,
        fixed = TRUE
      )
    } else {
      NULL
    }

    teal.transform::data_extract_spec(
      dataname = dataname,
      filter = cs_to_filter_spec(cs, multiple = multiple, label = label),
      select = select
    )
  } else {
    return(cs)
  }
}

#' Whether object is of class [teal.transform::choices_selected()] or [teal.transform::data_extract_spec()]
#'
#' @param x object to be checked
#'
#' @export
#' @return (`logical`)
is.cs_or_des <- function(x) { # nolint
  inherits(x, c("data_extract_spec", "choices_selected"))
}

#' Split-Column Expression
#'
#' Renders the expression for column split in `rtables` depending on:
#' - the expected or not arm comparison
#' - the expected or not arm combination
#'
#' @param compare (`logical`)\cr if `TRUE` the reference level is included.
#' @param combine (`logical`)\cr if `TRUE` the group combination is included.
#' @param ref (`character`)\cr the reference level (not used for `combine = TRUE`).
#' @param arm_var (`character`)\cr the arm or grouping variable name.
#'
#' @export
split_col_expr <- function(compare, combine, ref, arm_var) {
  if (compare && combine) {
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
  } else if (compare && !combine) {
    substitute(
      expr = rtables::split_cols_by(
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
      expr = rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var)
    )
  }
}

#' Split `choices_selected` objects with interactions into
#' their component variables
#'
#' @param x (`choices_selected`)\cr
#'   object with interaction terms
#'
#' @export
#' @note uses the regex `\\*|:` to perform the split.
split_choices <- function(x) {
  checkmate::assert_class(x, "choices_selected")
  checkmate::assert_character(x$choices, min.len = 1)

  split_x <- x
  split_x$choices <- split_interactions(x$choices)
  if (!is.null(x$selected)) {
    split_x$selected <- split_interactions(x$selected)
  }

  return(split_x)
}

#' Extracts html id for \code{data_extract_ui}
#' @description The \code{data_extract_ui} is located under extended html id.
#'   We could not use \code{ns("original id")} for reference, as it is extended with specific suffixes.
#' @param varname (`character`)\cr
#'   the original html id.  This should be retrieved with \code{ns("original id")} in the UI function
#'   or \code{session$ns("original id")}/"original id" in the server function.
#' @param dataname (`character`)\cr
#'   \code{dataname} from data_extract input.
#'   This might be retrieved like \code{data_extract_spec(...)[[1]]$dataname}.
#' @param filter optional, (`logical`)\cr
#'   if the connected \code{extract_data_spec} has objects passed to its `filter` argument
#'
#' @export
extract_input <- function(varname, dataname, filter = FALSE) {
  if (filter) {
    paste0(varname, "-dataset_", dataname, "_singleextract-filter1-vals")
  } else {
    paste0(varname, "-dataset_", dataname, "_singleextract-select")
  }
}

#' Split interaction terms into their component variables
#'
#' @param x (`character`)\cr
#'  string representing the interaction
#'  usually in the form `x:y` or `x*y`.
#' @param by (`character`)\cr
#'  regex with which to split the interaction
#'  term by.
#'
#' @export
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
#' (e.g. `ADSL` and `ADRS`).
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
#' @inheritParams template_arguments
#' @param ref_arm_val (`character`)\cr replacement name for the reference level.
#' @param drop (`logical`)\cr drop the unused variable levels.
#'
#' @export
#'
#' @examples
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
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    is.null(ref_arm) || is.character(ref_arm),
    is.character(comp_arm) || is.null(comp_arm),
    assertthat::is.flag(compare_arm),
    assertthat::is.string(ref_arm_val),
    assertthat::is.flag(drop)
  )

  data_list <- list()

  if (compare_arm) {
    # Data are filtered to keep only arms of interest.
    data_list <- add_expr(
      data_list,
      substitute(
        expr = dataname %>%
          dplyr::filter(arm_var %in% arm_val),
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
          expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = ref_arm, new_level = ref_arm_val)),
          names = list(arm_var = as.name(arm_var)),
          others = list(ref_arm = ref_arm, ref_arm_val = ref_arm_val)
        )
      )
    }

    # Reference level is explicit.
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = stats::relevel(arm_var, ref = ref_arm_val)),
        names = list(arm_var = as.name(arm_var)),
        others = list(ref_arm_val = ref_arm_val)
      )
    )
  } else {
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
        expr = dplyr::mutate(arm_var = droplevels(arm_var)),
        names = list(arm_var = as.name(arm_var))
      )
    )
  }

  pipe_expr(data_list)
}

#' Expression: Prepare Arm Levels
#'
#' This function generates the standard expression for pre-processing of dataset
#' arm levels in and is used to apply the same steps in safety teal modules.
#'
#' @inheritParams template_arguments
#'
#' @export
#' @examples
#' \dontrun{
#' teal.modules.clinical::prepare_arm_levels(
#'   dataname = "adae",
#'   parentname = "adsl",
#'   arm_var = "ARMCD",
#'   drop_arm_levels = TRUE
#' )
#'
#' teal.modules.clinical::prepare_arm_levels(
#'   dataname = "adae",
#'   parentname = "adsl",
#'   arm_var = "ARMCD",
#'   drop_arm_levels = FALSE
#' )
#' }
#'
prepare_arm_levels <- function(dataname,
                               parentname,
                               arm_var,
                               drop_arm_levels = TRUE) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.flag(drop_arm_levels)
  )

  data_list <- list()

  if (drop_arm_levels) {
    # Keep only levels that exist in `dataname` dataset
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = dataname <- dataname %>% dplyr::mutate(
          arm_var = droplevels(arm_var)
        ),
        names = list(
          dataname = as.name(dataname),
          arm_var = as.name(arm_var)
        )
      )
    )

    data_list <- add_expr(
      data_list,
      substitute(
        expr = arm_levels <- levels(dataname[[arm_var]]),
        env = list(
          dataname = as.name(dataname),
          arm_var = arm_var
        )
      )
    )

    # Data are filtered to keep only arms of interest.
    data_list <- add_expr(
      data_list,
      substitute(
        expr = parentname <- parentname %>%
          dplyr::filter(arm_var %in% arm_levels),
        env = list(
          parentname = as.name(parentname),
          arm_var = as.name(arm_var)
        )
      )
    )

    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = parentname <- parentname %>% dplyr::mutate(
          arm_var = droplevels(arm_var)
        ),
        names = list(
          parentname = as.name(parentname),
          arm_var = as.name(arm_var)
        )
      )
    )
  } else {
    # Keep only levels that exist in `parentname` dataset
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = parentname <- parentname %>% dplyr::mutate(
          arm_var = droplevels(arm_var)
        ),
        names = list(
          parentname = as.name(parentname),
          arm_var = as.name(arm_var)
        )
      )
    )

    data_list <- add_expr(
      data_list,
      substitute(
        expr = arm_levels <- levels(parentname[[arm_var]]),
        env = list(
          parentname = as.name(parentname),
          arm_var = arm_var
        )
      )
    )

    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = dataname <- dataname %>% dplyr::mutate(
          arm_var = factor(arm_var, levels = arm_levels)
        ),
        names = list(
          dataname = as.name(dataname),
          arm_var = as.name(arm_var)
        )
      )
    )
  }

  bracket_expr(data_list)
}

#' Mapping function for Laboratory Table
#'
#' Map value and level characters to values with with proper html tags, colors and icons.
#'
#' @param x (`character`)\cr vector with elements under the format (`value level`).
#' @param classes (`character`)\cr classes vector.
#' @param colors (`list`)\cr color per class.
#' @param default_color (`character`)\cr default color.
#' @param icons (`list`)\cr certain icons per level.
#'
#' @export
#'
color_lab_values <- function(x,
                             classes = c("HIGH", "NORMAL", "LOW"),
                             colors = list(HIGH = "red", NORMAL = "grey", LOW = "blue"),
                             default_color = "black",
                             icons = list(
                               HIGH = "glyphicon glyphicon-arrow-up",
                               LOW = "glyphicon glyphicon-arrow-down"
                             )) {
  is_character <- is.character(x) && is.vector(x)

  if ((!is_character) || !any(grepl(sprintf("(?:%s)", paste0(classes, collapse = "|")), x, perl = TRUE))) {
    x
  } else {
    vapply(x, function(val) {
      class <- classes[vapply(classes, function(class) {
        grepl(sprintf("%s", class), val)
      }, logical(1))]
      if (!is.null(class) & length(class) > 0) {
        color <- colors[class]
        if (is.null(color)) color <- default_color
        icony <- icons[class]
        value_val <- strsplit(val, " ")[[1]][1]
        sprintf("<span style='color:%s!important'>%s<i class='%s'></i></span>", color, value_val, icony)
      } else {
        val
      }
    }, character(1))
  }
}

#' Clean a categorical variable descriptions
#'
#' Cleaning categorical variable descriptions before presenting.
#'
#' @param x (`character`)\cr vector with categories descriptions.
#'
#' @export
#'
clean_description <- function(x) {
  x <- gsub("\\(.*?\\)", "", x)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- ifelse(nchar(x) > 20,
    yes = paste0(strtrim(x, width = 17), "..."),
    no = x
  )
  x
}


#' Utility function for extracting `paramcd` for forest plots
#'
#' Utility function for extracting `paramcd` for forest plots
#'
#' @param paramcd [`teal.transform::data_extract_spec()`]
#' variable value designating the studied parameter.
#'
#' @param input shiny app input
#'
#' @param filter_idx filter section index (default 1)
#' @keywords internal
#'
get_g_forest_obj_var_name <- function(paramcd, input, filter_idx = 1) {
  choices <- paramcd$filter[[filter_idx]]$choices
  input_obj <- paste0(
    "paramcd-dataset_", paramcd$dataname,
    "_singleextract-filter", filter_idx, "-vals"
  )
  current_selected <- input[[input_obj]]
  obj_var_name <- names(choices)[choices == current_selected]
  obj_var_name
}


#' Extract the associated parameter value for `paramcd`
#'
#' Utility function for extracting the parameter value that is associated
#' with the `paramcd` value label. If there is no parameter value for
#' the `paramcd` label, the `paramcd` value is returned. This is used
#' for generating the title.
#'
#' @param anl Analysis dataset
#'
#' @param paramcd [`teal.transform::data_extract_spec()`]
#' variable value designating the studied parameter.
#' @keywords internal
get_paramcd_label <- function(anl, paramcd) {
  positions <- grep(
    paste(unique(anl[[unlist(paramcd$filter)["vars_selected"]]]), collapse = "|"),
    names(unlist(paramcd$filter))
  )
  label_paramcd <- sapply(positions, function(pos) {
    if (nchar(sub(".*: ", "", names(unlist(paramcd$filter))[pos])) > 0) {
      label_paramcd <- sub(".*: ", "", names(unlist(paramcd$filter))[pos])
    } else {
      label_paramcd <- sub(":.*", "", names(unlist(paramcd$filter))[pos])
      label_paramcd <- sub(".*\\.", "", label_paramcd)
    }
    label_paramcd
  })
}

as_numeric_from_comma_sep_str <- function(input_string, sep = ",") {
  if (!is.null(input_string) && trimws(input_string) != "") {
    split_string <- unlist(strsplit(trimws(input_string), sep))
    split_as_numeric <- suppressWarnings(as.numeric(split_string))
  } else {
    split_as_numeric <- NULL
  }
  return(split_as_numeric)
}
