#' Observer for Treatment reference variable
#'
#' @description
#' Updates the reference and comparison Treatments when the selected Treatment variable changes
#'
#' @param session (`environment`)\cr shiny session
#' @param input (`character`)\cr shiny input
#' @param output (`character`)\cr shiny input
#' @param id_ref (`character`)\cr id of reference Treatment input UI element
#' @param id_comp (`character`)\cr id of comparison group input UI element
#' @param id_arm_var (`character`)\cr id of Treatment variable input UI element
#' @param data (`reactive` or `data.frame`)\cr dataset used to validate Treatment reference inputs and
#'   set `id_ref` input.
#' @param arm_ref_comp (`unknown`)\cr Treatment reference and compare variables provided as a
#'   nested list where each Treatment variable corresponds a list specifying the default levels for the
#'   reference and comparison treatments.
#' @param module (`character`)\cr name of the module where this is called (this is only used
#'   to produce more informative error messages)
#' @param on_off (`logical`)\cr A reactive that can be used to
#'   stop the whole observer if `FALSE`.
#' @param input_id (`character`)\cr unique id that the buckets will be referenced with.
#' @param output_id (`character`)\cr name of the UI id that the output will be written to.
#' @return Returns a `shinyvalidate::InputValidator` which checks that there is at least one reference
#'   and comparison arm
#' @keywords internal
#'
arm_ref_comp_observer <- function(session,
                                  input,
                                  output,
                                  id_ref = "Ref",
                                  id_comp = "Comp",
                                  id_arm_var,
                                  data,
                                  arm_ref_comp,
                                  module,
                                  on_off = reactive(TRUE),
                                  input_id = "buckets",
                                  output_id = "arms_buckets") {
  iv <- shinyvalidate::InputValidator$new()
  iv1 <- shinyvalidate::InputValidator$new()
  iv2 <- shinyvalidate::InputValidator$new()
  iv2$condition(~ iv1$is_valid())
  iv1$add_rule(id_arm_var, shinyvalidate::sv_required("Treatment variable must be selected"))
  iv2$add_rule(input_id, ~ if (length(arm_bucket_values(., id_ref)) == 0L) {
    "A reference arm must be selected"
  })
  iv2$add_rule(input_id, ~ if (length(arm_bucket_values(., id_comp)) == 0L) {
    "A comparison arm must be selected"
  })
  iv$add_validator(iv1)
  iv$add_validator(iv2)

  output[[output_id]] <- renderUI({
    if (isTRUE(on_off())) {
      df <- if (is.reactive(data)) {
        data()
      } else {
        data
      }
      check_arm_ref_comp(arm_ref_comp, df, module) ## throws an error if there are issues

      arm_var <- req(input[[id_arm_var]])

      arm <- df[[arm_var]]
      teal::validate_has_elements(arm, "Treatment variable is empty.")

      arm_levels <- if (is.factor(arm)) {
        levels(droplevels(arm))
      } else {
        unique(arm)
      }
      default_settings <- arm_ref_comp[[arm_var]]

      if (is.null(default_settings)) {
        ref_arm <- arm_levels[1]
        comp_arm <- setdiff(arm_levels, ref_arm)
      } else {
        ref_arm <- default_settings$ref
        comp_arm <- default_settings$comp
      }

      buckets <- list(ref_arm, comp_arm)
      names(buckets) <- c(id_ref, id_comp)

      teal.widgets::draggable_buckets(
        session$ns(input_id),
        label = "Groups",
        elements = character(),
        buckets = buckets
      )
    }
  })

  iv
}

#' Flatten draggable bucket contents to non-empty character arms
#'
#' Aligns with [teal::validate_has_elements()] used by [validate_standard_inputs()]:
#' list length can be positive while [unlist()] is empty (e.g. nested empty vectors).
#'
#' @param buckets (`list` or `NULL`)\cr value of Shiny `input$buckets`.
#' @param name (`character(1)`)\cr bucket id, typically `"Ref"` or `"Comp"`.
#'
#' @return `character()` (possibly length zero).
#' @keywords internal
#'
arm_bucket_values <- function(buckets, name) {
  checkmate::assert_string(name)
  if (is.null(buckets)) {
    return(character(0))
  }
  b <- buckets[[name]]
  if (is.null(b)) {
    return(character(0))
  }
  x <- unlist(b, recursive = TRUE, use.names = FALSE)
  if (length(x) == 0L) {
    return(character(0))
  }
  x <- as.character(x)
  x[!is.na(x) & nzchar(x)]
}

#' Shiny validator for reference / comparison arm buckets only
#'
#' For modules using [teal.picks::picks_ui()], treatment column selection is validated via
#' [teal.picks::picks_srv()] reactives; this validator only enforces `input$buckets` arms.
#'
#' @param id_ref (`character(1)`)\cr bucket name for reference arms.
#' @param id_comp (`character(1)`)\cr bucket name for comparison arms.
#'
#' @return A `shinyvalidate::InputValidator`.
#' @keywords internal
#'
arm_ref_comp_buckets_validator <- function(id_ref = "Ref", id_comp = "Comp") {
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("buckets", function(value) {
    if (length(arm_bucket_values(value, id_ref)) == 0L) {
      return("A reference arm must be selected")
    }
    NULL
  })
  iv$add_rule("buckets", function(value) {
    if (length(arm_bucket_values(value, id_comp)) == 0L) {
      return("A comparison arm must be selected")
    }
    NULL
  })
  iv
}

#' Observer for Treatment reference variable
#'
#' @description
#' Updates the reference and comparison Treatments when the selected Treatment variable changes
#'
#' @param session (`environment`)\cr shiny session
#' @param input (`character`)\cr shiny input
#' @param output (`character`)\cr shiny input
#' @param id_ref (`character`)\cr id of reference Treatment input UI element
#' @param id_comp (`character`)\cr id of comparison group input UI element
#' @param id_arm_var (`character`)\cr id of Treatment variable input UI element
#' @param data (`reactive` or `data.frame`)\cr dataset used to validate Treatment reference inputs and
#'   set `id_ref` input.
#' @param arm_ref_comp (`unknown`)\cr Treatment reference and compare variables provided as a
#'   nested list where each Treatment variable corresponds a list specifying the default levels for the
#'   reference and comparison treatments.
#' @param module (`character`)\cr name of the module where this is called (this is only used
#'   to produce more informative error messages)
#' @param on_off (`logical`)\cr A reactive that can be used to
#'   stop the whole observer if `FALSE`.
#' @param input_id (`character`)\cr unique id that the buckets will be referenced with.
#' @param output_id (`character`)\cr name of the UI id that the output will be written to.
#' @param arm_var_r (`reactive`)\cr reactive expression that returns the selected Treatment variable.
#' @return A `shiny::reactive` that runs arm/bucket validation (call inside other reactives before
#'   `teal::validate_inputs()`).
#' @keywords internal
#'
arm_ref_comp_observer_picks <- function(session, # nolint: object_name.
                                        input,
                                        output,
                                        id_ref = "Ref",
                                        id_comp = "Comp",
                                        id_arm_var,
                                        data,
                                        arm_ref_comp,
                                        module,
                                        on_off = reactive(TRUE),
                                        input_id = "buckets",
                                        output_id = "arms_buckets",
                                        arm_var_r) {
  output[[output_id]] <- renderUI({
    if (isTRUE(on_off())) {
      df <- if (is.reactive(data)) {
        data()
      } else {
        data
      }

      check_arm_ref_comp(arm_ref_comp, df, module) ## throws an error if there are issues

      arm_var_vec <- req(arm_var_r())
      arm_var <- arm_var_vec[[1]]

      arm <- df[[arm_var]]
      teal::validate_has_elements(arm, "Treatment variable is empty.")

      arm_levels <- if (is.factor(arm)) {
        levels(droplevels(arm))
      } else {
        unique(arm)
      }
      default_settings <- arm_ref_comp[[arm_var]]

      if (is.null(default_settings)) {
        ref_arm <- arm_levels[1]
        comp_arm <- setdiff(arm_levels, ref_arm)
      } else {
        ref_arm <- default_settings$ref
        comp_arm <- default_settings$comp
      }

      buckets <- list(ref_arm, comp_arm)
      names(buckets) <- c(id_ref, id_comp)

      teal.widgets::draggable_buckets(
        session$ns(input_id),
        label = "Groups",
        elements = character(),
        buckets = buckets
      )
    }
  })

  reactive({
    if (!isTRUE(on_off())) {
      return(invisible(NULL))
    }
    validate_input(
      inputId = id_arm_var,
      condition = length(arm_var_r()) >= 1L,
      message = "Treatment variable must be selected.",
      session = session
    )
    buckets_val <- input[[input_id]]
    validate_input(
      inputId = input_id,
      condition = length(arm_bucket_values(buckets_val, id_ref)) > 0L,
      message = "A reference arm must be selected.",
      session = session
    )
    validate_input(
      inputId = input_id,
      condition = length(arm_bucket_values(buckets_val, id_comp)) > 0L,
      message = "A comparison arm must be selected.",
      session = session
    )
  })
}

#' Check if the Treatment variable is reference or compare
#'
#' @description Check Treatment variable type.
#' @param x (`character`)\cr Name of the variable
#' @param df_to_check (`data.frame`)\cr table to check
#' @param module (`character`)\cr teal module the ref and comp are called in
#'
#' @keywords internal
#'
#' @return `TRUE` or `FALSE` whether the variable is in ref or comp
check_arm_ref_comp <- function(x, df_to_check, module) {
  msg <- paste("module", module, "argument arm_ref_comp ")

  if (!is.null(x)) {
    if (!is.list(x)) {
      stop(msg, "needs to be a list or NULL")
    }

    vars <- names(x)
    if (is.null(vars) || any(vars == "")) {
      stop(msg, "is not named")
    }

    if (!all(vars %in% names(df_to_check))) {
      stop(msg, "refers to variables that are not in the data")
    }

    Map(
      x, vars,
      f = function(xi, var) {
        if (!checkmate::check_list(xi) || !setequal(names(xi), c("comp", "ref"))) {
          stop(
            msg, "definition for Treatment variable ",
            var, " list element needs to be lists with ref and comp elements"
          )
        }
      }
    )
  }

  invisible(TRUE)
}
