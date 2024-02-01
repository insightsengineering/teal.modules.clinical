#' Teal Modules for Standard Clinical Outputs
#'
#' Provides teal modules for the standard clinical trials outputs.
#' The teal modules add an encoding panel to interactively change the encodings within teal.
#'
"_PACKAGE"

#' @import teal
#' @import teal.transform
#' @import tern
#' @importFrom dplyr %>% .data
#' @importFrom methods is
#' @importFrom tern.gee lsmeans

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
