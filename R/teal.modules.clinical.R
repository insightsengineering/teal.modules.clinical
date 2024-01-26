#' Teal Modules for Standard Clinical Outputs
#'
#' Provides teal modules for the standard clinical trials outputs.
#' The teal modules add an encoding panel to interactively change the encodings within teal.
#'
#' @import teal
#' @import teal.transform
#' @import tern
#' @importFrom dplyr %>%
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom tern.gee lsmeans
#'
#' @name teal.modules.clinical
#' @keywords internal
"_PACKAGE"

# We need this to avoid R CMD check warning about missing global definitions.
utils::globalVariables(c(
  "arm_var",
  "column_name",
  "n_column_name",
  "new_arm_ref_comp",
  "new_comp_arm",
  "usubjid",
  "x_var"
))
