#' Teal Modules for TLGs in tern
#'
#' @import shiny teal tern teal.devel utils.nest dplyr ggplot2
#' @importFrom methods is
"_PACKAGE"


#' @import assertthat
#' @import rtables
#'
NULL

# We need this to avoid R CMD check warning about missing global definitions.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "new_arm_ref_comp",
    "new_comp_arm",
    "usubjid"
  ))
}
