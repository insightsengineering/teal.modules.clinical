#' teal Module: Forest Survival Plot (teal.picks)
#'
#' This module produces a grid-style forest plot for time-to-event data with ADaM structure.
#' This is the `teal.picks` implementation of [tm_g_forest_tte()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_forest_tte
#' @param dataname (`character`)\cr name of the analysis dataset (e.g. `"ADTTE"`).
#' @param parentname (`character`)\cr name of the parent dataset (e.g. `"ADSL"`).
#' @param arm_var (`variables` or `NULL`)\cr variable spec for the treatment arm variable.
#'   Resolved against `parentname`.
#' @param paramcd_var (`variables`)\cr variable spec pointing at the `PARAMCD` column.
#'   Combined with `paramcd_value` to build a filter picks.
#' @param paramcd_value (`values`)\cr value filter spec to select the endpoint (PARAMCD level).
#' @param aval_var (`variables`)\cr variable spec for the analysis (time-to-event) variable (e.g. `AVAL`).
#'   Resolved against `dataname`. Fixed by default.
#' @param cnsr_var (`variables`)\cr variable spec for the censoring variable (e.g. `CNSR`).
#'   Resolved against `dataname`. Fixed by default.
#' @param time_unit_var (`variables`)\cr variable spec for the time unit variable (e.g. `AVALU`).
#'   Resolved against `dataname`. Fixed by default.
#' @param subgroup_var (`variables`)\cr variable spec for subgroup variables (multiple, ordered).
#'   Resolved against `parentname`.
#' @param strata_var (`variables`)\cr variable spec for stratification variables (multiple).
#'   Resolved against `parentname`.
#' @param conf_level (`values`)\cr available confidence levels.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_forest_tte.picks(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' @export
