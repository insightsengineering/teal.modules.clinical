#' Validate standard input values for a teal module
#'
#' @description Validates standard input.
#' @param adsl data.frame with subject-level data
#' @param adslvars required variables from `ADSL`
#' @param anl data.frame with analysis data
#' @param anlvars required variables from `ANL`
#' @param need_arm flag indicating whether grouping variable `arm_var`
#' is required or can be optionally `NULL`.
#' @param arm_var character with name of grouping variable, typically arm
#' @param ref_arm character with name of reference level in `arm_var`
#' @param comp_arm character with name for comparison level in `arm_var`
#' @param min_n_levels_armvar minimum number of levels in grouping variable `arm_var`.
#'   Defaults to 1, `NULL` for no minimum.
#' @param max_n_levels_armvar maximum number of levels in grouping variable `arm_var`.
#'   Use `NULL` for no maximum.
#' @param min_nrow minimum number of observations in `ADSL` and `ANL`
#'
#' @keywords internal
#'
validate_standard_inputs <- function(adsl,
                                     adslvars = character(0),
                                     anl,
                                     anlvars = character(0),
                                     need_arm = TRUE,
                                     arm_var,
                                     ref_arm,
                                     comp_arm,
                                     min_n_levels_armvar = 1L,
                                     max_n_levels_armvar = 100L,
                                     min_nrow = 1) {
  teal::validate_has_data(adsl, min_nrow = min_nrow)
  teal::validate_has_data(anl, min_nrow = min_nrow)

  if (length(adslvars) > 0) {
    teal::validate_has_variable(adsl, c(adslvars, arm_var))
  }
  if (length(anlvars) > 0) {
    teal::validate_has_variable(anl, anlvars)
  }

  if (need_arm || (!(need_arm) && !is.null(arm_var))) {
    teal::validate_has_elements(arm_var, "Treatment variable name is empty.")
    teal::validate_has_variable(adsl, arm_var, "Treatment variable not found.")

    validate_n_levels(
      adsl[[arm_var]],
      min_levels = min_n_levels_armvar,
      max_levels = max_n_levels_armvar,
      var_name = arm_var
    )

    validate_arm(adsl[[arm_var]])

    if (!missing(comp_arm)) {
      teal::validate_has_elements(comp_arm, "Comparison treatments selection is empty.")
    }
    if (!missing(ref_arm)) {
      teal::validate_has_elements(ref_arm, "Reference treatments selection is empty.")
    }

    if (!missing(comp_arm) && !missing(ref_arm)) {
      teal::validate_in(
        c(comp_arm, ref_arm), adsl[[arm_var]],
        "Current ADSL data does not have observations from the reference and comparison treatments."
      )
    }
  }
}

#' Check if vector is valid as to be used as a treatment arm variable
#'
#' @details A validate error is returned if the vector is not a factor with a more detailed
#'   error message if any of the entries are empty strings
#' @param arm_vec vector to be validated
#' @keywords internal
#'
validate_arm <- function(arm_vec) {
  validate(shiny::need(is.factor(arm_vec), "Treatment variable is not a factor"))
  validate(
    need(
      all(trimws(levels(arm_vec)) != ""),
      "Treatment values cannot contain empty strings"
    )
  )
}
