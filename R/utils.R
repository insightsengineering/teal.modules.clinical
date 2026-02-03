#' Concatenate expressions via a binary operator
#'
#' e.g. combine with `+` for `ggplot` without introducing parentheses due to associativity
#'
#' @param args arguments to concatenate with operator
#' @param bin_op binary operator to concatenate it with
#'
#' @return a `call`
#'
#' @examples
#' library(ggplot2)
#'
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
#'   list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg))))
#' )
#'
#' eval(
#'   call_concatenate(
#'     list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg))))
#'   )
#' )
#'
#' @export
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
#' @param datasets (`teal::FilteredData`)\cr Data built up by teal
#' @param dataname (`character`)\cr name of the dataset
#' @param vars (`character`)\cr Column names in the data
#'
#' @return `character` variable labels.
#'
#' @export
get_var_labels <- function(datasets, dataname, vars) {
  lifecycle::deprecate_stop(
    when = "0.8.14",
    what = "get_var_labels()",
    with = "teal.data::col_labels()",
    details = "teal.modules.clinical won't export any utility functions except those which
      are necessary to prepare shiny app."
  )
  labels <- datasets$get_varlabels(dataname, vars)
  labels <- vapply(vars, function(x) ifelse(is.na(labels[[x]]), x, labels[[x]]), character(1))
  labels
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
#' expr <- quote({
#'   library(rtables)
#'   basic_table() %>%
#'     split_cols_by(var = "ARMCD") %>%
#'     test_proportion_diff(
#'       vars = "rsp", method = "cmh", variables = list(strata = "strata")
#'     ) %>%
#'     build_table(df = dta)
#' })
#'
#' h_concat_expr(expr)
h_concat_expr <- function(expr) {
  expr <- deparse(expr)
  paste(expr, collapse = "\n")
}


#' Expressions as a Pipeline
#'
#' Concatenate expressions in a single pipeline-flavor expression.
#'
#' @param exprs (`list` of `call`)\cr expressions to concatenate in a
#'   pipeline (`%>%`).
#' @param pipe_str (`character`)\cr the character which separates the expressions.
#'
#' @return a `call`
#'
#' @examples
#' pipe_expr(
#'   list(
#'     expr1 = substitute(df),
#'     expr2 = substitute(head)
#'   )
#' )
#'
#' @export
pipe_expr <- function(exprs, pipe_str = "%>%") {
  exprs <- lapply(exprs, h_concat_expr)
  exprs <- unlist(exprs)
  exprs <- paste(exprs, collapse = pipe_str)
  str2lang(exprs)
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
#' @export
#'
#' @examples
#' library(rtables)
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
#'       vars = "rsp", method = "cmh", variables = list(strata = "strata")
#'     )
#'   )
#' )
#' lyt <- add_expr(lyt, quote(build_table(df = dta)))
#' pipe_expr(lyt)
add_expr <- function(expr_ls, new_expr) {
  checkmate::assert_list(expr_ls)
  checkmate::assert(is.call(new_expr) || is.name(new_expr))

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
#' @return a `{` object. See [base::Paren()] for details.
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
#'
#' @export
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
#' @param cs (`choices_selected`)\cr object to be transformed. See [teal.transform::choices_selected()] for details.
#' @param multiple (`logical`)\cr Whether multiple values shall be allowed in the
#'  shiny [shiny::selectInput()].
#' @param ordered (`logical(1)`)\cr Flags whether selection order should be tracked.
#' @param label (`character`)\cr Label to print over the selection field. For no label, set to `NULL`.
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
#' @param dataname (`character`)\cr name of the data
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
  if (!inherits(cs$selected, "delayed_data") && !multiple && length(cs$selected) != 1 && !is.null(cs$selected)) {
    stop(cs_name, " must only have 1 selected value")
  }

  if (inherits(cs, "choices_selected")) {
    teal.transform::data_extract_spec(
      dataname = dataname,
      select = cs_to_select_spec(cs, multiple = multiple, ordered = ordered, label = label)
    )
  } else {
    cs
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
    cs
  }
}

#' Whether object is of class [teal.transform::choices_selected()]
#'
#' @param x object to be checked
#'
#' @export
#' @return (`logical`)
is.cs_or_des <- function(x) { # nolint: object_name.
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
#' @return a `call`
#'
#' @examples
#' split_col_expr(
#'   compare = TRUE,
#'   combine = FALSE,
#'   ref = "ARM A",
#'   arm_var = "ARMCD"
#' )
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
#' @note uses the regex `\\*|:` to perform the split.
#'
#' @return  a [teal.transform::choices_selected()] object.
#'
#' @examples
#' split_choices(choices_selected(choices = c("x:y", "a*b"), selected = all_choices()))
#'
#' @export
split_choices <- function(x) {
  checkmate::assert_class(x, "choices_selected")
  checkmate::assert_character(x$choices, min.len = 1)

  split_x <- x
  split_x$choices <- split_interactions(x$choices)
  if (!is.null(x$selected)) {
    split_x$selected <- split_interactions(x$selected)
  }

  split_x
}

#' Extracts html id for `data_extract_ui`
#'
#' The `data_extract_ui` is located under extended html id. We could not use `ns("original id")`
#' for reference, as it is extended with specific suffixes.
#'
#' @param varname (`character`)\cr
#'   the original html id.  This should be retrieved with `ns("original id")` in the UI function
#'   or `session$ns("original id")`/"original id" in the server function.
#' @param dataname (`character`)\cr
#'   `dataname` from data_extract input.
#'   This might be retrieved like `data_extract_spec(...)[[1]]$dataname`.
#' @param filter (`logical`) optional,\cr
#'   if the connected `extract_data_spec` has objects passed to its `filter` argument
#'
#' @return a string
#'
#' @examples
#' extract_input("ARM", "ADSL")
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
#' @return a vector of strings where each element is a component
#'   variable extracted from interaction term `x`.
#'
#' @examples
#' split_interactions("x:y")
#' split_interactions("x*y")
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
#' @return a `call`
#'
#' @examples
#' prepare_arm(
#'   dataname = "adrs",
#'   arm_var = "ARMCD",
#'   ref_arm = "ARM A",
#'   comp_arm = c("ARM B", "ARM C")
#' )
#'
#' prepare_arm(
#'   dataname = "adsl",
#'   arm_var = "ARMCD",
#'   ref_arm = c("ARM B", "ARM C"),
#'   comp_arm = "ARM A"
#' )
#'
#' @export
prepare_arm <- function(dataname,
                        arm_var,
                        ref_arm,
                        comp_arm,
                        compare_arm = !is.null(ref_arm),
                        ref_arm_val = paste(ref_arm, collapse = "/"),
                        drop = TRUE) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(arm_var)
  checkmate::assert_character(ref_arm, null.ok = TRUE)
  checkmate::assert_character(comp_arm, null.ok = TRUE)
  checkmate::assert_flag(compare_arm)
  checkmate::assert_string(ref_arm_val)
  checkmate::assert_flag(drop)

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
          expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, levels = ref_arm, new_level = ref_arm_val)),
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
#' @return a `{` object. See [base::Paren()] for details.
#'
#' @examples
#' prepare_arm_levels(
#'   dataname = "adae",
#'   parentname = "adsl",
#'   arm_var = "ARMCD",
#'   drop_arm_levels = TRUE
#' )
#'
#' prepare_arm_levels(
#'   dataname = "adae",
#'   parentname = "adsl",
#'   arm_var = "ARMCD",
#'   drop_arm_levels = FALSE
#' )
#'
#' @export
prepare_arm_levels <- function(dataname,
                               parentname,
                               arm_var,
                               drop_arm_levels = TRUE) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_flag(drop_arm_levels)

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
#' @return a character vector where each element is a formatted HTML tag corresponding to
#'   a value in `x`.
#'
#' @examples
#' color_lab_values(c("LOW", "LOW", "HIGH", "NORMAL", "HIGH"))
#'
#' @export
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

#' Clean up categorical variable description
#'
#' Cleaning categorical variable descriptions before presenting.
#'
#' @param x (`character`)\cr vector with categories descriptions.
#'
#' @return a string
#'
#' @examples
#' clean_description("Level A (other text)")
#' clean_description("A long string that should be shortened")
#'
#' @export
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
  input_obj <- paste0(
    "paramcd-dataset_", paramcd$dataname,
    "_singleextract-filter", filter_idx, "-vals"
  )

  current_selected <- input[[input_obj]]
  choices <- paramcd$filter[[filter_idx]]$choices
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
  split_as_numeric
}

#' Default string for total column label
#'
#' @description
#'
#' The default string used as a label for the "total" column. This value is used as the default
#' value for the `total_label` argument throughout the `teal.modules.clinical` package. If not specified
#' for each module by the user via the `total_label` argument, or in the R environment options via
#' [set_default_total_label()], then `"All Patients"` is used.
#'
#' @param total_label (`string`)\cr Single string value to set in the R environment options as
#'   the default label to use for the "total" column. Use `getOption("tmc_default_total_label")` to
#'   check the current value set in the R environment (defaults to `"All Patients"` if not set).
#'
#' @name default_total_label
NULL

#' @describeIn default_total_label Getter for default total column label.
#'
#' @return
#' * `default_total_label` returns the current value if an R environment option has been set
#'   for `"tmc_default_total_label"`, or `"All Patients"` otherwise.
#'
#' @examples
#' # Default settings
#' default_total_label()
#' getOption("tmc_default_total_label")
#'
#' # Set custom value
#' set_default_total_label("All Patients")
#'
#' # Settings after value has been set
#' default_total_label()
#' getOption("tmc_default_total_label")
#'
#' @export
default_total_label <- function() {
  getOption("tmc_default_total_label", default = "All Patients")
}

#' @describeIn default_total_label Setter for default total column label. Sets the
#'   option `"tmc_default_total_label"` within the R environment.
#'
#' @return
#' * `set_default_total_label` has no return value.
#'
#' @export
set_default_total_label <- function(total_label) {
  checkmate::assert_character(total_label, len = 1, null.ok = TRUE)
  options("tmc_default_total_label" = total_label)
}

# for mocking in tests
interactive <- NULL

#' Wrappers around `srv_transform_teal_data` that allows to decorate the data
#' @inheritParams teal::srv_transform_teal_data
#' @param expr (`expression` or `reactive`) to evaluate on the output of the decoration.
#' When an expression it must be inline code. See [within()]
#' Default is `NULL` which won't evaluate any appending code.
#' @param expr_is_reactive (`logical(1)`) whether `expr` is a reactive expression
#' that skips defusing the argument.
#' @details
#' `srv_decorate_teal_data` is a wrapper around `srv_transform_teal_data` that
#' allows to decorate the data with additional expressions.
#' When original `teal_data` object is in error state, it will show that error
#' first.
#'
#' @keywords internal
srv_decorate_teal_data <- function(id, data, decorators, expr, expr_is_reactive = FALSE) {
  checkmate::assert_class(data, classes = "reactive")
  checkmate::assert_list(decorators, "teal_transform_module")
  checkmate::assert_flag(expr_is_reactive)

  missing_expr <- missing(expr)
  if (!missing_expr && !expr_is_reactive) {
    expr <- dplyr::enexpr(expr) # Using dplyr re-export to avoid adding rlang to Imports
  }

  moduleServer(id, function(input, output, session) {
    decorated_output <- srv_transform_teal_data("inner", data = data, transformators = decorators)

    reactive({
      data_out <- try(data(), silent = TRUE)
      if (inherits(data_out, "qenv.error")) {
        data()
      } else {
        # ensure original errors are displayed and `eval_code` is never executed with NULL
        req(data(), decorated_output())
        if (missing_expr) {
          decorated_output()
        } else if (expr_is_reactive) {
          teal.code::eval_code(decorated_output(), expr())
        } else {
          teal.code::eval_code(decorated_output(), expr)
        }
      }
    })
  })
}

#' @rdname srv_decorate_teal_data
#' @details
#' `ui_decorate_teal_data` is a wrapper around `ui_transform_teal_data`.
#' @keywords internal
ui_decorate_teal_data <- function(id, decorators, ...) {
  teal::ui_transform_teal_data(NS(id, "inner"), transformators = decorators, ...)
}

#' Internal function to check if decorators is a valid object
#' @noRd
check_decorators <- function(x, names = NULL) { # nolint: object_name.

  check_message <- checkmate::check_list(x, names = "named")

  if (!is.null(names) && isTRUE(check_message)) {
    if (length(names(x)) != length(unique(names(x)))) {
      check_message <- sprintf(
        "The `decorators` must contain unique names from these names: %s.",
        paste(names, collapse = ", ")
      )
    } else if (!all(unique(names(x)) %in% names)) {
      check_message <- sprintf(
        "The `decorators` must be a named list from these names: %s.",
        paste(names, collapse = ", ")
      )
    }
  }

  if (!isTRUE(check_message)) {
    return(check_message)
  }

  valid_elements <- vapply(
    x,
    checkmate::test_class,
    classes = "teal_transform_module",
    FUN.VALUE = logical(1L)
  )

  if (all(valid_elements)) {
    return(TRUE)
  }

  "Make sure that the named list contains 'teal_transform_module' objects created using `teal_transform_module()`."
}
#' Internal assertion on decorators
#' @noRd
assert_decorators <- checkmate::makeAssertionFunction(check_decorators)

#' Subset decorators based on the scope
#'
#' @param scope (`character`) a character vector of decorator names to include.
#' @param decorators (named `list`) of list decorators to subset.
#'
#' @return Subsetted list with all decorators to include.
#' It can be an empty list if none of the scope exists in `decorators` argument.
#' @keywords internal
select_decorators <- function(decorators, scope) {
  checkmate::assert_character(scope, null.ok = TRUE)
  if (scope %in% names(decorators)) {
    decorators[scope]
  } else {
    list()
  }
}

#' Set the attributes of the last chunk outputs
#'
#' This function modifies the attributes of the last `n` elements of a `teal_card`
#' that are `chunk_output` objects. It can be used to set attributes like `dev.width`
#' and `dev.height` for plot outputs.
#'
#' @param teal_card (`teal_card`) the teal_card object to modify
#' @param attributes (`list`) named list of attributes to set
#' @param n (`integer(1)`) number of the last element of `teal_card` to modify.
#' it will only change `chunk_output` objects.
#' @param inner_classes (`character`) classes within `chunk_output` that should be modified.
#' This can be used to only change `recordedplot`, `ggplot2` or other type of objects.
#' @param quiet (`logical`) whether to suppress warnings
#' @keywords internal
set_chunk_attrs <- function(teal_card,
                            attributes,
                            n = 1,
                            inner_classes = NULL,
                            quiet = FALSE) {
  checkmate::assert_class(teal_card, "teal_card")
  checkmate::assert_list(attributes, names = "unique")
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_character(inner_classes, null.ok = TRUE)
  checkmate::assert_flag(quiet)

  if (!inherits(teal_card[[length(teal_card)]], "chunk_output")) {
    if (!quiet) {
      warning("The last element of the `teal_card` is not a `chunk_output` object. No attributes were modified.")
    }
    return(teal_card)
  }

  for (ix in seq_along(teal_card)) {
    if (ix > n) {
      break
    }
    current_ix <- length(teal_card) + 1 - ix
    if (!inherits(teal_card[[current_ix]], "chunk_output")) {
      if (!quiet) {
        warning(
          "The ", ix,
          " to last element of the `teal_card` is not a `chunk_output` object. Skipping any further modifications."
        )
      }
      return(teal_card)
    }

    if (
      length(inner_classes) > 0 &&
        length(teal_card[[current_ix]]) >= 1 &&
        !checkmate::test_multi_class(teal_card[[current_ix]][[1]], inner_classes)
    ) {
      next
    }

    attributes(teal_card[[current_ix]]) <- utils::modifyList(
      attributes(teal_card[[current_ix]]),
      attributes
    )
  }

  teal_card
}

#' Create a reactive that sets plot dimensions on a `teal_card`
#'
#' This is a convenience function that creates a reactive expression that
#' automatically sets the `dev.width` and `dev.height` attributes on the last
#' chunk outputs of a `teal_card` based on plot dimensions from a plot widget.
#'
#' @param pws (`plot_widget`) plot widget that provides dimensions via `dim()` method
#' @param q_r (`reactive`) reactive expression that returns a `teal_reporter`
#' @param inner_classes (`character`) classes within `chunk_output` that should be modified.
#' This can be used to only change `recordedplot`, `ggplot2` or other type of objects.
#'
#' @return A reactive expression that returns the `teal_card` with updated dimensions
#'
#' @keywords internal
set_chunk_dims <- function(pws, q_r, inner_classes = NULL) {
  checkmate::assert_list(pws)
  checkmate::assert_names(names(pws), must.include = "dim")
  checkmate::assert_class(pws$dim, "reactive")
  checkmate::assert_class(q_r, "reactive")
  checkmate::assert_character(inner_classes, null.ok = TRUE)

  reactive({
    pws_dim <- stats::setNames(as.list(req(pws$dim())), c("width", "height"))
    if (identical(pws_dim$width, "auto")) { # ignore non-numeric values (such as "auto")
      pws_dim$width <- NULL
    }
    if (identical(pws_dim$height, "auto")) { # ignore non-numeric values (such as "auto")
      pws_dim$height <- NULL
    }
    q <- req(q_r())
    teal.reporter::teal_card(q) <- set_chunk_attrs(
      teal.reporter::teal_card(q),
      list(dev.width = pws_dim$width, dev.height = pws_dim$height),
      inner_classes = inner_classes
    )
    q
  })
}
