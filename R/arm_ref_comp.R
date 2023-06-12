#' Observer for Treatment reference variable
#'
#' @description `r lifecycle::badge("stable")`
#' Updates the reference and comparison Treatments when the selected Treatment variable changes
#'
#' @param session (`environment`) shiny session
#' @param input (`character`) shiny input
#' @param output (`character`) shiny input
#' @param id_ref (`character`) id of reference Treatment input UI element
#' @param id_comp (`character`) id of comparison group input UI element
#' @param id_arm_var (`character`) id of Treatment variable input UI element
#' @param data (`reactive` or `data.frame`) dataset used to validate Treatment reference inputs and
#'   set `id_ref` input.
#' @param arm_ref_comp (`unknown`) Treatment reference and compare variables provided as a
#'   nested list where each Treatment variable corresponds a list specifying the default levels for the
#'   reference and comparison treatments.
#' @param module (`character`) name of the module where this is called (this is only used
#'   to produce more informative error messages)
#' @param on_off (`logical`) A reactive that can be used to
#'   stop the whole observer if FALSE.
#' @param input_id (`character`) unique id that the buckets will be referenced with.
#' @param output_id (`character`) name of the UI id that the output will be written to.
#' @return Returns a `shinyvalidate::InputValidator` which checks that there is at least one reference
#'   and comparison arm
#' @keywords internal
#'
#' @examples
#'
#' arm_ref_comp <- list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))
#' arm_var <- choices_selected(c("ARM", "ARMCD"), "ARMCD")
#'
#' adsl <- data.frame(ARM = c("ARM 1", "ARM 2"), ARMCD = c("ARM A", "ARM B"))
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       teal.widgets::optionalSelectInput(
#'         "arm",
#'         "Treatment Variable",
#'         choices = arm_var$choices,
#'         selected = arm_var$selected
#'       ),
#'       shiny::uiOutput("arms_buckets")
#'     ),
#'     mainPanel(
#'       shiny::textOutput("result")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   iv_arm_ref <- teal.modules.clinical:::arm_ref_comp_observer(
#'     session,
#'     input,
#'     output,
#'     id_arm_var = "arm",
#'     data = adsl,
#'     arm_ref_comp = arm_ref_comp,
#'     module = "example"
#'   )
#'
#'   output$result <- shiny::renderText({
#'     iv <- shinyvalidate::InputValidator$new()
#'     iv$add_validator(iv_arm_ref)
#'     iv$enable()
#'     teal::validate_inputs(iv)
#'     "Valid selection has been made!"
#'   })
#' }
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
arm_ref_comp_observer <- function(session,
                                  input,
                                  output,
                                  id_ref = "Ref",
                                  id_comp = "Comp",
                                  id_arm_var,
                                  data,
                                  arm_ref_comp,
                                  module,
                                  on_off = shiny::reactive(TRUE),
                                  input_id = "buckets",
                                  output_id = "arms_buckets") {
  iv <- shinyvalidate::InputValidator$new()
  iv1 <- shinyvalidate::InputValidator$new()
  iv2 <- shinyvalidate::InputValidator$new()
  iv2$condition(~ iv1$is_valid())
  iv1$add_rule(id_arm_var, shinyvalidate::sv_required("Treatment variable must be selected"))
  iv2$add_rule(input_id, ~ if (length(.[[id_ref]]) == 0) "A reference arm must be selected")
  iv2$add_rule(input_id, ~ if (length(.[[id_comp]]) == 0) "A comparison arm must be selected")
  iv$add_validator(iv1)
  iv$add_validator(iv2)


  output[[output_id]] <- shiny::renderUI({
    if (isTRUE(on_off())) {
      df <- if (shiny::is.reactive(data)) {
        data()
      } else {
        data
      }
      check_arm_ref_comp(arm_ref_comp, df, module) ## throws an error if there are issues

      arm_var <- shiny::req(input[[id_arm_var]])

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

  return(iv)
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
