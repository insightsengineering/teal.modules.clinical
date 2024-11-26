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
#' @param add_total (`logical`)\cr whether to include column with total number of patients.
#' @param anl_name (`character`)\cr analysis data used in teal module.
#' @param arm_var (`character`)\cr variable names that can be used as `arm_var`.
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param aval `r lifecycle::badge("deprecated")` Please use the `aval_var` argument instead.
#' @param avalu `r lifecycle::badge("deprecated")` Please use the `avalu_var` argument instead.
#' @param avalu_var (`character`)\cr name of the analysis value unit variable.
#' @param aval_var (`character`)\cr name of the analysis value variable.
#' @param baseline_var (`character`)\cr name of the variable for baseline values of the analysis variable.
#' @param base_var `r lifecycle::badge("deprecated")` Please use the `baseline_var` argument instead.
#' @param basic_table_args (`basic_table_args`) optional\cr object created by [teal.widgets::basic_table_args()]
#'   with settings for the module table. The argument is merged with option `teal.basic_table_args` and with default
#'   module arguments (hard coded in the module body).
#'   For more details, see the vignette: `vignette("custom-basic-table-arguments", package = "teal.widgets")`.
#' @param by_vars (`character`)\cr variable names used to split the summary by rows.
#' @param cmdecod (`character`)\cr name of standardized medication name variable.
#' @param cmindc (`character`)\cr name of indication variable.
#' @param cmstdy (`character`)\cr name of study relative day of start of medication variable.
#' @param cnsr_var (`character`)\cr name of the censoring variable.
#' @param combine_comp_arms (`logical`)\cr triggers the combination of comparison arms.
#' @param compare_arm (`logical`)\cr triggers the comparison between study arms.
#' @param comp_arm (`character`)\cr the level of comparison arm in case of arm comparison.
#' @param conf_level (`numeric`)\cr value for the confidence level within the range of (0, 1).
#' @param control (`list`)\cr list of settings for the analysis.
#' @param cov_var (`character`)\cr names of the covariates variables.
#' @param dataname (`character`)\cr analysis data used in teal module.
#' @param denominator (`character`)\cr chooses how percentages are calculated. With option `N`, the reference
#'   population from the column total is used as the denominator. With option `n`, the number of non-missing
#'   records in this row and column intersection is used as the denominator. If `omit` is chosen, then the
#'   percentage is omitted.
#' @param drop_arm_levels (`logical`)\cr whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var` levels are
#'   set to those used in the `dataname` dataset. If `FALSE`, `arm_var` levels are set to those used in the
#'   `parentname` dataset. If `dataname` and `parentname` are the same, then `drop_arm_levels` is set to `TRUE` and
#'   user input for this parameter is ignored.
#' @param event_type (`character`)\cr type of event that is summarized (e.g. adverse event, treatment). Default
#'   is `"event"`.
#' @param font_size (`numeric`)\cr font size value.
#' @param ggplot2_args (`ggplot2_args`) optional\cr object created by [teal.widgets::ggplot2_args()] with settings
#'   for the module plot. The argument is merged with option `teal.ggplot2_args` and with default module arguments
#'   (hard coded in the module body).
#'   For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#' @param hlt (`character`)\cr name of the variable with high level term for events.
#' @param id_var (`character`)\cr the variable name for subject id.
#' @param include_interact (`logical`)\cr whether an interaction term should be included in the model.
#' @param label_hlt (`string`)\cr label of the `hlt` variable from `dataname`. The label will be extracted from the
#'   module.
#' @param label_llt (`string`)\cr label of the `llt` variable from `dataname`. The label will be extracted from the
#'   module.
#' @param llt (`character`)\cr name of the variable with low level term for events.
#' @param patient_id (`character`)\cr patient ID.
#' @param na_level (`string`)\cr used to replace all `NA` or empty values
#'   in character or factor variables in the data. Defaults to `"<Missing>"`. To set a
#'   default `na_level` to apply in all modules, run `set_default_na_str("new_default")`.
#' @param na.rm (`logical`)\cr whether `NA` values should be removed prior to analysis.
#' @param numeric_stats (`character`)\cr names of statistics to display for numeric summary variables. Available
#'   statistics are `n`, `mean_sd`, `mean_ci`, `median`, `median_ci`, `quantiles`, `range`, and `geom_mean`.
#' @param paramcd (`character`)\cr name of the parameter code variable.
#' @param parentname (`character`)\cr parent analysis data used in teal module, usually this refers to `ADSL`.
#' @param patient_id (`character`)\cr patient ID.
#' @param prune_diff (`number`)\cr threshold to use for trimming table using as criteria difference in
#'   rates between any two columns.
#' @param prune_freq (`number`)\cr threshold to use for trimming table using event incidence rate in any column.
#' @param ref_arm (`character`)\cr the level of reference arm in case of arm comparison.
#' @param sort_criteria (`character`)\cr how to sort the final table. Default option `freq_desc` sorts
#'   on column `sort_freq_col` by decreasing number of patients with event. Alternative option `alpha` sorts events
#'   alphabetically.
#' @param strata_var (`character`)\cr names of the variables for stratified analysis.
#' @param subgroup_var (`character`)\cr with variable names that can be used as subgroups.
#' @param sum_vars (`character`)\cr names of the variables that should be summarized.
#' @param time_points (`character`)\cr time points that can be used in [tern::surv_timepoint()].
#' @param time_unit_var (`character`)\cr name of the variable representing time units.
#' @param title (`character`)\cr title of the output.
#' @param total_label (`string`)\cr string to display as total column/row label if column/row is
#'   enabled (see `add_total`). Defaults to `"All Patients"`. To set a new default `total_label` to
#'   apply in all modules, run `set_default_total_label("new_default")`.
#' @param treatment_flag (`character`)\cr name of the value indicating on treatment
#'   records in `treatment_flag_var`.
#' @param treatment_flag_var (`character`)\cr name of the on treatment flag variable.
#' @param useNA (`character`)\cr whether missing data (`NA`) should be displayed as a level.
#' @param var_labels (named `character`) optional\cr variable labels for relabeling the analysis variables.
#' @param visit_var (`character`)\cr variable names that can be used as `visit` variable. Must be a factor in
#'   `dataname`.
#' @param worst_flag_indicator (`character`)\cr value indicating worst grade.
#' @param worst_flag_var (`character`)\cr name of the worst flag variable.
#'
#' @return a `list` of expressions to generate a table or plot object.
#'
#' @name template_arguments
#' @keywords internal
NULL

#' Standard Module Arguments
#'
#' The documentation to this function lists all the arguments in teal modules
#' that are used repeatedly to express an analysis.
#'
#' @details
#' * Although this function just returns `NULL` it has two uses, for
#'   the teal module users it provides a documentation of arguments that are
#'   commonly and consistently used in the framework. For the developer it adds a
#'   single reference point to import the `roxygen` argument description with:
#'   `@inheritParams module_arguments`
#' * Parameters with identical descriptions & input types to those in the Standard Template Arguments section are
#'   excluded to reduce duplication as each module function inherits parameters from its corresponding template
#'   function.
#'
#' @param arm_ref_comp (`list`) optional,\cr if specified it must be a named list with each element corresponding to
#'   an arm variable in `ADSL` and the element must be another list (possibly
#'   with delayed [teal.transform::variable_choices()] or delayed [teal.transform::value_choices()]
#'   with the elements named `ref` and `comp` that the defined the default
#'   reference and comparison arms when the arm variable is changed.
#' @param arm_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable in the results table.
#' @param atirel ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `ATIREL` variable from `dataname`.
#' @param aval_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and pre-selected option for the analysis variable.
#' @param avalu_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for the analysis unit variable.
#' @param avisit ([teal.transform::choices_selected()])\cr value of analysis
#'   visit `AVISIT` of interest.
#' @param baseline_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for variable values that can be used as `baseline_var`.
#' @param by_vars ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names used to split the summary by rows.
#' @param cmdecod ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `CMDECOD` variable from `dataname`.
#' @param cmindc ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `CMINDC` variable from `dataname`.
#' @param cmstdy ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `CMSTDY` variable from `dataname`.
#' @param cnsr_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the censoring variable.
#' @param conf_level ([teal.transform::choices_selected()])\cr object with
#'   all available choices and pre-selected option for the confidence level, each within range of (0, 1).
#' @param cov_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the covariates variables.
#' @param dataname (`character`)\cr analysis data used in teal module.
#' @param default_responses (`list` or `character`)\cr defines
#'   the default codes for the response variable in the module per value of `paramcd`.
#'   A passed vector is transmitted for all `paramcd` values. A passed `list` must be named
#'   and contain arrays, each name corresponding to a single value of `paramcd`. Each array
#'   may contain default response values or named arrays `rsp` of default selected response
#'   values and `levels` of default level choices.
#' @param fixed_symbol_size (`logical`)\cr When (`TRUE`), the same symbol size is used for plotting each estimate.
#'   Otherwise, the symbol size will be proportional to the sample size in each each subgroup.
#' @param font_size (`numeric`)\cr numeric vector of length 3 of current, minimum and maximum font size values.
#' @param hlt ([teal.transform::choices_selected()])\cr name of the variable
#'   with high level term for events.
#' @param id_var ([teal.transform::choices_selected()])\cr object specifying
#'   the variable name for subject id.
#' @param interact_var (`character`)\cr name of the variable that should have interactions
#'   with arm. If the interaction is not needed, the default option is `NULL`.
#' @param interact_y (`character`)\cr a selected item from the interact_var column which will be used
#'   to select the specific `ANCOVA` results when interact_var is discrete. If the interaction is not
#'   needed, the default option is `FALSE`.
#' @param label (`character`)\cr menu item label of the module in the teal app.
#' @param llt ([teal.transform::choices_selected()])\cr name of the variable
#'   with low level term for events.
#' @param paramcd ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the parameter code variable from `dataname`.
#' @param parentname (`character`)\cr parent analysis data used in teal module, usually this refers to `ADSL`.
#' @param patient_col (`character`)\cr name of patient ID variable.
#' @param plot_height (`numeric`) optional\cr vector of length three with `c(value, min, max)`. Specifies the
#'   height of the main plot and renders a slider on the plot to interactively adjust the plot height.
#' @param plot_width (`numeric`) optional\cr vector of length three with `c(value, min, max)`. Specifies the width
#'   of the main plot and renders a slider on the plot to interactively adjust the plot width.
#' @param post_output (`shiny.tag`) optional,\cr with text placed after the output to put the output into context.
#'   For example the [shiny::helpText()] elements are useful.
#' @param pre_output (`shiny.tag`) optional,\cr with text placed before the output to put the output into context.
#'   For example a title.
#' @param strata_var ([teal.transform::choices_selected()])\cr names of
#'   the variables for stratified analysis.
#' @param summarize_vars ([teal.transform::choices_selected()])\cr names of
#'   the variables that should be summarized.
#' @param subgroup_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for variable names that can be used as the default subgroups.
#' @param time_points ([teal.transform::choices_selected()])\cr object with all available choices and preselected option
#'   for time points that can be used in [tern::surv_timepoint()].
#' @param time_unit_var ([teal.transform::choices_selected()])\cr object
#'   with all available choices and pre-selected option for the time unit variable.
#' @param treatment_flag ([teal.transform::choices_selected()])\cr value
#'   indicating on treatment records in `treatment_flag_var`.
#' @param treatment_flag_var ([teal.transform::choices_selected()])\cr on
#'   treatment flag variable.
#' @param useNA (`character`)\cr whether missing data (`NA`) should be displayed as a level.
#' @param visit_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for variable names that can be used as `visit` variable.
#'   Must be a factor in `dataname`.
#' @param worst_flag_indicator ([teal.transform::choices_selected()])\cr
#'   value indicating worst grade.
#' @param worst_flag_var ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for variable names that can be used as worst flag variable.
#' @param decorators `r lifecycle::badge("experimental")` (`list` of `teal_transform_module` or `NULL`) optional,
#' if not `NULL`, decorator for tables or plots included in the module.
#'
#' @return a `teal_module` object.
#'
#' @seealso The [TLG Catalog](https://insightsengineering.github.io/tlg-catalog/stable/) where additional example
#'   apps implementing this module can be found.
#'
#' @name module_arguments
#' @keywords internal
NULL
