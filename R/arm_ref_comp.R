#' Observer for Treatment reference variable
#'
#' @description `r lifecycle::badge("stable")`
#' Updates the reference and comparison Treatments when the selected Treatment variable changes
#'
#' @param session (`environment`) shiny session
#' @param input (`character`) shiny input
#' @param output (`character`) shiny input
#' @param id_ref (`character`) id of reference Treatment input ui element
#' @param id_comp (`character`) id of comparison group input ui element
#' @param id_arm_var (`character`) id of Treatment variable input ui element
#' @param datasets (`FilteredData`) object from the module
#' @param dataname (`character`) dataset name
#' @param arm_ref_comp (`unknown`) Treatment reference and compare variables provided as a
#'   nested list where each Treatment variable corresponds a list specifying the default levels for the
#'   reference and comparison treatments.
#' @param module (`character`) name of the module where this is called (this is only used
#'   to produce more informative error messages)
#' @param on_off (`logical`) A reactive that can be used to
#'   stop the whole observer if FALSE.
#' @param input_id (`character`) unique id that the buckets will be referenced with.
#' @param output_id (`character`) name of the UI id that the output will be written to.
#'
#' @keywords internal
#'
#' @examples
#' ds <- teal:::get_dummy_datasets()
#'
#' arm_ref_comp <- list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))
#' arm_var <- choices_selected(c("ARM", "ARMCD"), "ARM")
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     teal.widgets::optionalSelectInput(
#'       "arm",
#'       "Treatment Variable",
#'       choices = arm_var$choices,
#'       selected = arm_var$selected
#'     ),
#'     uiOutput("arms_buckets"),
#'   ),
#'   server = function(input, output, session) {
#'     shiny::isolate({
#'       teal.modules.clinical:::arm_ref_comp_observer(
#'         session,
#'         input,
#'         output,
#'         id_ref = "ref",
#'         id_comp = "comp",
#'         id_arm_var = "arm",
#'         datasets = ds,
#'         arm_ref_comp = arm_ref_comp,
#'         module = "example"
#'       )
#'     })
#'   }
#' )
#' }
arm_ref_comp_observer <- function(session,
                                  input,
                                  output,
                                  id_ref = "Ref",
                                  id_comp = "Comp",
                                  id_arm_var,
                                  datasets,
                                  dataname = "ADSL",
                                  arm_ref_comp,
                                  module,
                                  on_off = shiny::reactive(TRUE),
                                  input_id = "buckets",
                                  output_id = "arms_buckets") {
  if (any(unlist(lapply(arm_ref_comp, lapply, inherits, "delayed_data")))) {
    stopifnot(
      all(vapply(arm_ref_comp, function(x) identical(sort(names(x)), c("comp", "ref")), logical(1)))
    )
    # when a delayed object is used for arm_ref_comp, the entire FilteredData
    # object must be passed to resolve it
    arm_ref_comp <- teal.transform::resolve_delayed(arm_ref_comp, datasets)
  }

  df <- datasets$get_data(dataname, filtered = FALSE)

  check_arm_ref_comp(arm_ref_comp, df, module) ## throws an error if there are issues

  # uses observe because observeEvent evaluates only when on_off() is switched
  # not necessarily when variables are dropped
  output[[output_id]] <- renderUI({
    if (!is.null(on_off()) && on_off()) {
      arm_var <- input[[id_arm_var]]

      # validations here don't produce nice UI message (it's observe and not render output) but it prevent red errors
      teal::validate_has_elements(arm_var, "Treatment variable name is empty.")

      arm <- df[[arm_var]]
      teal::validate_has_elements(arm, "Treatment variable is empty.")

      arm_levels <- if (is.factor(arm)) {
        levels(arm)
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
}

#' Check if the Treatment variable is reference or compare
#'
#' @description `r lifecycle::badge("stable")`
#' @param x (\code{character}) Name of the variable
#' @param df_to_check (\code{data.frame}) table to check
#' @param module \code{character} teal module the ref and comp are called in
#'
#' @keywords internal
#'
#' @return \code{TRUE} or \code{FALSE} whether the variable is in ref or comp
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


    Map(function(xi, var) {
      if (!is.list(xi)) {
        stop(
          msg, "definition for Treatment variable ",
          var, " list element needs to be lists with ref and comp elements"
        )
      }

      rc <- names(xi)
      if (is.null(rc) || !identical(sort(rc), c("comp", "ref"))) {
        stop(msg, "definition for Treatment variable ", var, " nested list needs to have the elements ref and comp")
      }


      arm_levels <- unlist(xi)

      if (!all(arm_levels %in% df_to_check[[var]])) {
        stop(
          msg, "definition for Treatment variable ",
          var, " refers to treatment levels that do not exist in the data"
        )
      }
    }, x, vars)
  }

  invisible(TRUE)
}
