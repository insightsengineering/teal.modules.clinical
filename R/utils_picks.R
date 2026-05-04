#' Coerce legacy `teal.transform` specs to [`teal.picks::variables()`] with deprecation
#'
#' If `x` is a legacy `choices_selected`, `filter_spec`, or `select_spec` object, it is converted
#' via [`teal.picks::as.picks()`]. Otherwise `x` must already inherit `"variables"`.
#'
#' @param x (`values`, `choices_selected` or `picks`) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object.
#' @param null.ok (`logical(1)`) whether `NULL` is allowed.
#'
#' @keywords internal
#' @noRd
migrate_choices_selected_to_variables <- function(x, # nolint: object_length_linter
                                                  arg_name = checkmate::vname(x),
                                                  multiple = NULL,
                                                  null.ok = FALSE) { # nolint: object_name_linter.
  # nolint: object_name_linter.
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(multiple, null.ok = TRUE)
  checkmate::assert_flag(null.ok)
  if (inherits(x, "picks")) {
    return(x)
  }

  if (isTRUE(null.ok) && is.null(x)) {
    return(x)
  }
  legacy <- c("choices_selected", "filter_spec", "select_spec")
  if (inherits(x, legacy)) {
    lifecycle::deprecate_warn(
      when = "0.13.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::variables()` (or a full `teal.picks::picks()` chain).",
        "Support for legacy `teal.transform::choices_selected()`, `filter_spec`, and `select_spec` is deprecated."
      )
    )
    x <- teal.picks::as.picks(x, quiet = FALSE)
    attr(x, "multiple") <- (!is.null(multiple) && multiple) || (is.null(multiple) && length(x$selected) > 1L)
  } else {
    if (!is.null(multiple) && !identical(attr(x, "multiple", exact = TRUE), multiple)) {
      stop(
        sprintf("`multiple` metadata does not match the requirement for %s.", arg_name),
        sprintf(" Please set multiple = %s in the picks object.", multiple)
      )
    }
  }
  checkmate::assert_class(
    x,
    "variables",
    null.ok = null.ok,
    .var.name = arg_name
  )
  x
}

#' Coerce legacy `choices_selected` to [`teal.picks::values()`] with deprecation
#'
#' @param x (`values`, `choices_selected` or `picks`) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object.
#'
#' @keywords internal
#' @noRd
migrate_choices_selected_to_values <- function(x, # nolint: object_length_linter
                                               arg_name = checkmate::vname(x),
                                               multiple = NULL) {
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(multiple, null.ok = TRUE)

  if (inherits(x, "picks")) {
    return(x)
  }
  if (inherits(x, "choices_selected")) {
    lifecycle::deprecate_warn(
      when = "0.13.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::values()`.",
        "Support for legacy `teal.transform::choices_selected()` is deprecated."
      )
    )
    if (is.null(x$choices) || inherits(x$choices, "delayed_data")) {
      stop(
        "Delayed `choices_selected` objects cannot be coerced automatically; ",
        "specify `teal.picks::values()` explicitly.",
        call. = FALSE
      )
    }
    choices <- as.character(x$choices)
    selected <- as.character(unlist(x$selected, use.names = FALSE))
    checkmate::assert_character(choices, min.len = 1L)
    checkmate::assert_character(selected, min.len = 1L)
    fixed <- isTRUE(x$fixed)
    multiple <- (!is.null(multiple) && multiple) || (is.null(multiple) && length(selected) > 1L)
    x <- teal.picks::values(choices, selected, fixed = fixed, multiple = multiple)
  }
  checkmate::assert_class(x, "values", .var.name = arg_name)
  x
}

#' Coerce legacy `choices_selected`-based specs to `picks` with deprecation
#'
#' @param x (`values`, `choices_selected` or `picks`) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object.
#'
#' @keywords internal
#' @noRd
migrate_value_choices_to_picks <- function(x, # nolint: object_length_linter.
                                           multiple = NULL,
                                           arg_name = checkmate::vname(x)) {
  if (inherits(x, "picks")) {
    if (!is.null(multiple) && !identical(attr(x$values, "multiple", exact = TRUE), multiple)) {
      stop(
        sprintf("`multiple` metadata does not match the requirement for %s.", arg_name),
        sprintf(" Please set multiple = %s in the picks object.", multiple)
      )
    }
    return(x)
  }

  values <- migrate_choices_selected_to_values(x, multiple = multiple, arg_name = arg_name)
  variable_name <- attr(x$choices, "var_choices", exact = TRUE)
  if (inherits(x, "choices_selected") && is.null(variable_name)) {
    stop(
      sprintf("When using choices_selected for %s", arg_name),
      " it should have 'var_choices' attribute specifying variable choices.",
      " Cannot convert to picks object without this information.",
      call. = FALSE
    )
  }
  teal.picks::picks(
    teal.picks::variables(variable_name, variable_name),
    values,
    check_dataset = FALSE
  )
}

create_picks_helper <- function(datasets = NULL, x) {
  if (inherits(x, "picks") && !is.null(x$datasets)) {
    return(x)
  }
  checkmate::assert_class(datasets, "datasets", null.ok = FALSE)
  checkmate::assert_multi_class(x, c("pick", "picks"))

  if (inherits(x, "picks")) {
    picks_args <- list(datasets, x$variables, x$values)
    do.call(
      teal.picks::picks,
      picks_args[vapply(picks_args, Negate(is.null), logical(1L))],
    )
  } else if (inherits(x, "pick")) {
    teal.picks::picks(datasets, x)
  }
}

#' Coerce legacy `data_extract_spec` / lists of specs to [`teal.picks::picks()`]
#'
#' Single-spec encodings become [`teal.picks::as.picks()`] output. Multiple
#' `data_extract_spec` objects (legacy list inputs) are combined into one
#' `picks()` with a [`teal.picks::datasets()`] step when choices are eager;
#' delayed specs must be replaced with explicit [`teal.picks::picks()`].
#'
#' @param x (`NULL`, `picks`, `data_extract_spec`, or `list` of `data_extract_spec`).
#' @param arg_name (`character(1)`) argument name for messages.
#' @param allow_null (`logical(1)`).
#'
#' @return `NULL` or a [`teal.picks::picks`] object.
#'
#' @keywords internal
#' @noRd
migrate_list_extract_spec_to_picks <- function(x,
                                               arg_name = "x",
                                               allow_null = TRUE) {
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(allow_null)
  if (isTRUE(allow_null) && is.null(x)) {
    return(x)
  }
  if (inherits(x, "picks")) {
    return(x)
  }

  des_list <- teal.transform::list_extract_spec(x, allow_null = allow_null)
  if (is.null(des_list)) {
    return(NULL)
  }

  legacy <- vapply(
    des_list,
    function(des) inherits(des, "data_extract_spec"),
    logical(1L)
  )
  if (any(legacy)) {
    lifecycle::deprecate_warn(
      when = "0.13.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::picks()` built with `teal.picks::datasets()` and `teal.picks::variables()`.",
        "Support for legacy `teal.transform::data_extract_spec()` is deprecated."
      )
    )
  }

  picks_one <- lapply(des_list, teal.picks::as.picks, quiet = FALSE)
  picks_one <- Filter(Negate(is.null), picks_one)
  checkmate::assert_list(picks_one, min.len = 1L, .var.name = arg_name)

  if (length(picks_one) == 1L) {
    return(picks_one[[1L]])
  }

  datanames <- unique(vapply(des_list, `[[`, character(1L), "dataname"))
  var_lists <- lapply(des_list, function(des) {
    ch <- des$select$choices
    if (checkmate::test_character(ch, min.len = 1L)) {
      ch
    } else {
      NULL
    }
  })
  if (any(vapply(var_lists, is.null, logical(1L)))) {
    stop(
      "Combining multiple `data_extract_spec` into picks requires eager character ",
      "`select_spec(choices = ...)` for `",
      arg_name,
      "`. Specify `teal.picks::picks()` explicitly for delayed or mixed specs.",
      call. = FALSE
    )
  }
  var_union <- sort(unique(unlist(var_lists, use.names = FALSE)))
  pick_selected <- vapply(
    des_list,
    function(des) {
      s <- des$select$selected
      if (checkmate::test_character(s, min.len = 1L)) {
        s[[1L]]
      } else {
        NA_character_
      }
    },
    character(1L)
  )
  pick_selected <- pick_selected[!is.na(pick_selected) & nzchar(pick_selected)]
  default_sel <- if (length(pick_selected) > 0L) {
    pick_selected[[length(pick_selected)]]
  } else {
    var_union[[1L]]
  }

  teal.picks::picks(
    teal.picks::datasets(choices = datanames),
    teal.picks::variables(
      choices = var_union,
      selected = default_sel,
      multiple = FALSE
    )
  )
}
