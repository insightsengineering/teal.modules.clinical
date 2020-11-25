#' Standard Arguments
#'
#' The documentation to this function lists all the arguments in teal modules
#' that are used repeatedly to express an analysis.
#'
#' @details Although this function just returns `NULL` it has two uses, for
#' the teal module users it provides a documentation of arguments that are
#' commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the `roxygen` argument description with:
#' `@inheritParams argument_convention`
#'
#' @param arm_var (`string` or `choices_selected` with teal module)\cr
#'   variable names designating the study arm.
#' @param arm_ref_comp (`choices_selected`, optional)\cr
#'   returned by [teal::choices_selected]. If specified it must be a named list
#'   with each element corresponding to an arm variable in `ADSL` and the
#'   element must be another list with the elements named `ref` and `comp`
#'   that the defined the default reference and comparison arms when the arm
#'   variable is changed.
#' @param id_var (`choices_selected` with teal module)\cr object specifying the
#'   variable name for subject id.
#' @param combine_arm (`flag`)\cr re-arrange arms in two groups, i.e. reference and treatment.
#' @param sum_vars (`character`)\cr names of the variables that should be summarized.
#' @param compare_arm (`flag`)\cr triggers the comparison between study arms.
#' @param combine_comp_arms (`flag`)\cr triggers the combination of comparison arms.
#' @param parentname (`string`)\cr parent analysis data used in teal module,
#'   usually this refers to `"ADSL"`.
#' @param parent_name (`string`)\cr parent analysis data used in teal module,
#'   usually this refers to `"ADSL"`.
#' @param dataname (`string`)\cr analysis data used in teal module.
#' @param anl_name (`string`)\cr analysis data used in teal module.
#' @param label (\code{character})\cr menu item label of the module in the teal app
#' @param param (`string` or `choices_selected` with teal module)\cr
#'   variable value designating the studied parameter.
#' @param paramcd (`string` or `choices_selected` with teal module)\cr
#'   variable value designating the studied parameter.
#' @param ref_arm (`string`)\cr the level of reference arm in case of
#'   arm comparison.
#' @param comp_arm (`character`)\cr the level of comparison arm in case of
#'   arm comparison.
#' @param strata_var (`string` or `choices_selected` with teal module)\cr
#'   names of the variables for stratified analysis.
#' @param na.rm (`flag`)\cr whether `NA` values should be removed prior to analysis.
#' @param useNA (`string`)\cr choose whether missing data (`NA`) should be displayed as a level.
#' @param denominator (`string`)\cr chooses how percentages are calculated.
#'   With option `N`, the reference population from the column total is used as
#'   the denominator. With option `n`, the number of non-missing records in this row and column
#'   intersection is used as the denominator. If `omit` is chosen, then the percentage is omitted.
#' @param var_labels (named `character`)\cr optional variable labels for relabeling the analysis variables.
#' @param avisit (`string` or `choices_selected` with teal module)\cr
#'   value of analysis visit `AVISIT` of interest.
#' @param cov_var (`string` or `choices_selected` with teal module)\cr
#'   names of the variables for covariates.
#' @param aval_var (`string` or `choices_selected` with teal module)\cr
#'   name of the variable for the analysis.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#'   Default is 0.95.
#' @param add_total (`flag`)\cr whether to include column with total number of patients.
#' @param hlt (`string` or `choices_selected` with teal module)\cr
#'   name of the variable with high level term for events.
#' @param llt (`string` or `choices_selected` with teal module)\cr
#'   name of the variable with low level term for events.
#' @param visit_var [teal::choices_selected] object with all available choices and preselected option
#'   for variable names that can be used as `visit` variable. Must be a factor in `dataname`.
#' @param by_vars \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names used to split the summary by rows.
#' @param subgroup_var \code{\link[teal]{choices_selected}} object with all available choices and
#'   preselected option for variable names that can be used as the default subgroups.
#' @param plot_height `numeric` vector to indicate default value, minimum and maximum values.
#' @param plot_width `numeric` vector to indicate default value, minimum and maximum values.
#' @name argument_convention
#'
NULL
