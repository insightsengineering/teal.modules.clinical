#' Teal Modules for Standard Clinical Outputs
#'
#' Provides teal modules for the standard clinical trials outputs.
#' The teal modules add an encoding panel to interactively change the encodings within teal.
#'
#' @import teal
#' @import tern
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#'
#' @docType package
#' @name teal.modules.clinical
#' @keywords internal
NULL

# We need this to avoid R CMD check warning about missing global definitions.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "new_arm_ref_comp",
    "new_comp_arm",
    "usubjid"
  ))
}

#' Avoiding R CMD check notes
#' @importFrom methods is
#' @importFrom rlang .data
#' @noRd
NULL
