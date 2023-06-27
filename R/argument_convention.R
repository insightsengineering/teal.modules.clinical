#' Standard Template Arguments
#'
#' The documentation to this function lists all the arguments in teal module
#' templates that are used repeatedly to express an analysis.
#'
#' @details Although this function just returns `NULL` it has two uses, for
#' the teal module users it provides a documentation of arguments that are
#' commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the `roxygen` argument description with:
#' `@inheritParams template_arguments`
#'
#' @param arm_var (`character`)\cr
#'   variable names that can be used as `arm_var`.
#' @param id_var (`character`)\cr
#'   the variable name for subject id.
#' @param sum_vars (`character`)\cr
#'   names of the variables that should be summarized.
#' @param compare_arm (`logical`)\cr
#'   triggers the comparison between study arms.
#' @param combine_comp_arms (`logical`)\cr
#'   triggers the combination of comparison arms.
#' @param parentname (`character`)\cr
#'   parent analysis data used in teal module, usually this refers to `ADSL`.
#' @param dataname (`character`)\cr
#'   analysis data used in teal module.
#' @param anl_name (`character`)\cr
#'   analysis data used in teal module.
#' @param paramcd (`character`)\cr
#'   variable value designating the studied parameter.
#' @param ref_arm (`character`)\cr
#'   the level of reference arm in case of arm comparison.
#' @param comp_arm (`character`)\cr
#'   the level of comparison arm in case of arm comparison.
#' @param na.rm (`logical`)\cr
#'   whether `NA` values should be removed prior to analysis.
#' @param useNA (`character`)\cr
#'   whether missing data (`NA`) should be displayed as a level.
#' @param na_level (`string`)\cr used to replace all `NA` or empty values
#'   in character or factor variables in the data.
#' @param denominator (`character`)\cr
#'   chooses how percentages are calculated. With option `N`, the reference
#'   population from the column total is used as the denominator. With option
#'   `n`, the number of non-missing records in this row and column intersection
#'   is used as the denominator. If `omit` is chosen, then the percentage is
#'   omitted.
#' @param var_labels optional, (named `character`)\cr
#'   variable labels for relabeling the analysis variables.
#' @param cov_var (`character`)\cr
#'   names of the covariates variables.
#' @param aval_var (`character`)\cr
#'   name of the analysis variable.
#' @param cnsr_var (`character`)\cr
#'   name of the censoring variable.
#' @param conf_level (`numeric`)\cr
#'   value for the confidence level within the range of (0, 1).
#' @param add_total (`logical`)\cr
#'   whether to include column with total number of patients.
#' @param hlt (`character`)\cr
#'   name of the variable with high level term for events.
#' @param llt (`character`)\cr
#'   name of the variable with low level term for events.
#' @param visit_var (`character`)\cr
#'   variable names that can be used as `visit` variable. Must be a factor in `dataname`.
#' @param include_interact (`logical`)\cr whether an interaction term should be included in the model.
#' @param interact_var (`character`)\cr name of the variable that should have interactions
#'   with arm. If the interaction is not needed, the default option is `NULL`.
#' @param interact_y (`character`)\cr a selected item from the interact_var column which will be used to select
#'   the specific `ANCOVA` results. If the interaction is not needed, the default option is `FALSE`.
#' @param by_vars (`character`)\cr
#'   variable names used to split the summary by rows.
#' @param time_points (`character`)\cr
#'   time points that can be used in [tern::surv_timepoint()].
#' @param time_unit_var (`character`)\cr
#'   name of the variable representing time units.
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parantname` dataset.
#' @param subgroup_var (`character`)\cr with variable names that can be used as subgroups.
#' @param strata_var (`character`)\cr
#'   names of the variables for stratified analysis.
#' @param title (`character`)\cr
#'   title of the output.
#' @param prune_freq (`number`)\cr threshold to use for trimming table using event incidence rate in any column.
#' @param prune_diff (`number`)\cr threshold to use for trimming table using as criteria difference in
#'   rates between any two columns.
#' @param treatment_flag_var (`character`)\cr name of the on treatment flag variable.
#' @param treatment_flag (`character`)\cr name of the value indicating on treatment
#'   records in `treatment_flag_var`.
#' @param basic_table_args optional, (`basic_table_args`)\cr
#'   object created by [teal.widgets::basic_table_args()] with settings for the module table.
#'   The argument is merged with option `teal.basic_table_args` and with default module arguments
#'   (hard coded in the module body).
#'
#'   For more details, see the vignette: `vignette("custom-basic-table-arguments", package = "teal.widgets")`.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#'   object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#'   The argument is merged with option `teal.ggplot2_args` and with default module arguments
#'   (hard coded in the module body).
#'
#'   For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#' @name template_arguments
#' @keywords internal
#'
NULL


#' Standard Module Arguments
#'
#' The documentation to this function lists all the arguments in teal modules
#' that are used repeatedly to express an analysis.
#'
#' @details Although this function just returns `NULL` it has two uses, for
#' the teal module users it provides a documentation of arguments that are
#' commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the `roxygen` argument description with:
#' `@inheritParams module_arguments`
#'
#' @param arm_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices
#'   and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param arm_ref_comp optional, (`list`)\cr
#'   If specified it must be a named list with each element corresponding to
#'   an arm variable in `ADSL` and the element must be another list (possibly
#'   with delayed [teal.transform::variable_choices()] or delayed [teal.transform::value_choices()]
#'   with the elements named `ref` and `comp` that the defined the default
#'   reference and comparison arms when the arm variable is changed.
#' @param id_var (`choices_selected` or `data_extract_spec`)\cr
#'   object specifying the variable name for subject id.
#' @param summarize_vars (`choices_selected` or `data_extract_spec`)\cr
#'   names of the variables that should be summarized.
#' @param strata_var (`choices_selected` or `data_extract_spec`)\cr
#'   names of the variables for stratified analysis.
#' @param parentname (`character`)\cr
#'   parent analysis data used in teal module, usually this refers to `ADSL`.
#' @param dataname (`character`)\cr
#'   analysis data used in teal module.
#' @param label (`character`)\cr
#'   menu item label of the module in the teal app.
#' @param paramcd (`choices_selected` or `data_extract_spec`)\cr
#'   variable value designating the studied parameter.
#' @param useNA (`character`)\cr
#'   whether missing data (`NA`) should be displayed as a level.
#' @param na_level (`string`)\cr used to replace all `NA` or empty values
#'   in character or factor variables in the data.
#' @param denominator (`character`)\cr
#'   chooses how percentages are calculated. With option `N`, the reference
#'   population from the column total is used as the denominator. With option
#'   `n`, the number of non-missing records in this row and column intersection
#'   is used as the denominator. If `omit` is chosen, then the percentage is omitted.
#' @param avisit (`choices_selected` or `data_extract_spec`)\cr
#'   value of analysis visit `AVISIT` of interest.
#' @param cov_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option for the covariates
#'   variables.
#' @param aval_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option for the analysis
#'   variable.
#' @param cnsr_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option for the censoring
#'   variable.
#' @param conf_level (`choices_selected`)\cr
#'   object with all available choices and preselected option for the confidence
#'   level, each within range of (0, 1).
#' @param add_total (`logical`)\cr
#'   whether to include column with total number of patients.
#' @param hlt (`choices_selected` or `data_extract_spec`)\cr
#'   name of the variable with high level term for events.
#' @param llt (`choices_selected` or `data_extract_spec`)\cr
#'   name of the variable with low level term for events.
#' @param visit_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used as `visit` variable. Must be a factor
#'   in `dataname`.
#' @param total_label (`string`)\cr string to display as total column/row label if column/row is
#'   enabled (see `add_total`).
#' @param include_interact (`logical`)\cr whether an interaction term should be included in the model.
#' @param interact_var (`character`)\cr name of the variable that should have interactions
#'   with arm. If the interaction is not needed, the default option is `NULL`.
#' @param interact_y (`character`)\cr a selected item from the interact_var column which will be used
#'   to select the specific `ANCOVA` results when interact_var is discrete. If the interaction is not
#'   needed, the default option is `FALSE`.
#' @param by_vars (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option for variable names
#'   used to split the summary by rows.
#' @param plot_height optional, (`numeric`)\cr
#'   a vector of length three with `c(value, min, max)`. Specifies the height of
#'   the main plot and renders a slider on the plot to interactively adjust the plot
#'   height.
#' @param plot_width optional, (`numeric`)\cr
#'   a vector of length three with `c(value, min, max)`. Specifies the width of
#'   the main plot and renders a slider on the plot to interactively adjust the plot
#'   width.
#' @param time_points (`choices_selected`)\cr
#'   object with all available choices and preselected option for time points that
#'   can be used in [tern::surv_timepoint()].
#' @param time_unit_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option for the time unit
#'   variable.
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parantname` dataset.
#' @param pre_output optional, (`shiny.tag`)\cr
#'   with text placed before the output to put the output into context.
#'   For example a title.
#' @param post_output optional, (`shiny.tag`)\cr
#'   with text placed after the output to put the output into context. For example
#'   the [shiny::helpText()] elements are useful.
#' @param subgroup_var (`choices_selected` or `data_extract_spec`)\cr
#'   object with all available choices and preselected option for variable names that can be used
#'   as the default subgroups.
#' @param treatment_flag_var (`choices_selected` or `data_extract_spec`)\cr on treatment flag variable.
#' @param treatment_flag (`choices_selected`] or `data_extract_spec`)\cr value indicating on treatment
#'   records in `treatment_flag_var`.
#' @param basic_table_args optional, (`basic_table_args`)\cr
#'   object created by [teal.widgets::basic_table_args()] with settings for the module table.
#'   The argument is merged with option `teal.basic_table_args` and with default module arguments
#'   (hard coded in the module body).
#'
#'   For more details, see the vignette: `vignette("custom-basic-table-arguments", package = "teal.widgets")`.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#'   object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#'   The argument is merged with option `teal.ggplot2_args` and with default module arguments
#'   (hard coded in the module body).
#'
#'   For more details, see the help vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @return `module` object generated by the [teal::module()] function.
#'
#' @name module_arguments
#' @keywords internal
NULL
