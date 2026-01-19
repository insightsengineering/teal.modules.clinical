# Changelog

## teal.modules.clinical 0.12.0.9003

## teal.modules.clinical 0.12.0

CRAN release: 2025-12-04

#### Enhancements

- Modules return a `teal_report` object that contains the data, code and
  report. All the reporter buttons were removed from the modules’ UI in
  favor of the framework level reporter buttons
  ([\#1376](https://github.com/insightsengineering/teal.modules.clinical/issues/1376)).
- Added `xticks` parameter to `tm_g_km` module to allow users to specify
  x-axis tick positions programmatically. When `xticks` is provided, the
  interactive input field is pre-populated with the specified values as
  a default. Users can then modify these values interactively, and their
  changes will take precedence over the default. When `xticks` is `NULL`
  (default), users can specify this interactively in the module.
- Updated `R` version dependency from 4.0 to 4.4.0, as upstream
  dependency `tern` requires this.

#### Bug fixes

- Modules `tm_t_summary` and `tm_t_summary_by` now display and hide
  missing values
  ([\#1437](https://github.com/insightsengineering/teal.modules.clinical/issues/1437)).

## teal.modules.clinical 0.11.1

CRAN release: 2025-08-22

#### Enhancements

- Improved the layout and appearance of the app using `bslib`
  components.
- Added argument `conf_type` to module `tm_g_km` to allow users to set a
  confidence interval type for median survival time estimation.
- Added `denom` argument in `tm_t_binary_outcome` module.
- Update `withr` dependency version.

#### Bug fixes

- Fixed bug in `tm_t_events` to return sorted table
  ([\#1393](https://github.com/insightsengineering/teal.modules.clinical/issues/1393)).
- Fixed “Add to Report” failures in `tm_g_pp_adverse_events`,
  `tm_g_pp_therapy`, `tm_t_pp_basic_info`, `tm_t_pp_laboratory` and
  `tm_t_pp_prior_medication` modules
  ([\#1404](https://github.com/insightsengineering/teal.modules.clinical/issues/1404)).

#### Breaking changes

- Removed the `table` object decoration in `tm_g_pp_adverse_events`,
  `tm_g_pp_therapy`
  ([\#1404](https://github.com/insightsengineering/teal.modules.clinical/issues/1404)).
- Deprecated the `decorators` argument in `tm_t_pp_basic_info`,
  `tm_t_pp_laboratory` and `tm_t_pp_prior_medication`
  ([\#1404](https://github.com/insightsengineering/teal.modules.clinical/issues/1404)).

## teal.modules.clinical 0.10.0

CRAN release: 2025-02-28

#### Enhancements

- Added `teal.logger` functionality for logging changes in shiny inputs
  in all modules.
- Introduced `ylim` parameter for `tm_g_km` module that controls width
  of y-axis.
- Added functionality to `tm_t_events_patyear` to split columns by
  multiple (nested) variables via the `arm_var` argument.
- Added arguments `arm_var_labels` to `template_summary` and
  `show_arm_var_labels` to `tm_t_summary` to allow user to display arm
  variable (`arm_var`) labels in table header.
- Added argument `stats` to modules `tm_g_forest_rsp` and
  `tm_g_forest_tte` to allow users to specify statistics to include in
  the table.
- Added argument `riskdiff` to modules `tm_g_forest_rsp` and
  `tm_g_forest_tte` to allow users to add a risk difference table
  column.
- Added the `count_dth` and `count_wd` parameters to
  `tm_t_events_summary` to select/deselect the “Total number of deaths”
  and “Total number of patients withdrawn from study due to an AE” rows,
  respectively. These options correspond to the “Count deaths” and
  “Count withdrawals due to AE” checkboxes available when the module is
  run.
- Added the `title_text` argument to `tm_t_mult_events` to allow for
  more customization of the module’s dynamic title.
- Introduced `transformators` and `decorators` argument to modules.

#### Bug fixes

- Fixed bug in creating modules with `delayed_data` and
  [`teal.transform::all_choices`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/delayed_choices.html).
- Fixed bug in `tm_g_forest_tte` and `tm_g_forest_rsp` to resolve
  delayed data loading variables.

#### Miscellaneous

- Removed `Show Warnings` modals from modules.
- Clarified the documentation specifying whether multiple values can be
  selected in the `arm_var` argument for each module.
- Replaced use of the
  [`rtables::add_colcounts()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_colcounts.html)
  function with the `show_colcounts` argument to
  [`basic_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html).
- Began deprecation cycle for the `show_labels` argument of
  `template_summary` which has no effect on the `tm_t_summary` module.
- Replaced instances of deprecated `strata` argument to
  [`tern::control_lineplot_vars()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_lineplot_vars.html)
  with `group_var`.
- Added an assertion to
  [`tm_t_events_summary()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_events_summary.md)
  to check whether all datasets used have the same treatment variable
  levels.

## teal.modules.clinical 0.9.1

CRAN release: 2024-04-27

#### Enhancements

- Updated `tm_g_forest_rsp` and `tm_g_forest_tte` to use refactored
  version of `g_forest`. Plots are now displayed as `ggplot` objects
  instead of `grob` objects. Added parameters `font_size` and
  `rel_width_forest` to control font size and width of plot relative to
  table, respectively.
- Updated `tm_t_summary_by` to allow `NULL` input to `paramcd` argument.
- Updated `tm_g_km` to use refactored version of `g_km`. Plots are now
  displayed as `ggplot` objects instead of `grob` objects. Added
  parameters `rel_height_plot`, `font_size`, `control_annot_surv_med`,
  and `control_annot_coxph` to control height of plot relative to table,
  font size, median survival time table size, and Cox-PH table size,
  respectively.
- Added the `control` argument to `tm_t_binary_outcome` to control
  settings for the analysis (methods, confidence intervals, and odds
  ratios) within the module.

#### Miscellaneous

- Replaced instances of deprecated `na_level` argument to `tern`
  functions with `na_str`.
- Replaced argument/list element name `strata` instead of `strat` in all
  `tern` function calls following the deprecation of this argument/name
  within `tern`.
- Removed `formatters` from dependencies and replaced the use of its
  functions relating to variable labels with functions from `teal.data`.

## teal.modules.clinical 0.9.0

CRAN release: 2024-02-23

#### Breaking Changes

- Adapted all modules to use `teal_data` objects.
- Module arguments that previously accepted inputs from
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  or
  [`teal.transform::data_extract_spec()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html)
  now only accept input from
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html).
  The affected modules are: `tm_a_gee`, `tm_a_mmrm`, `tm_g_ci`,
  `tm_g_forest_rsp`, `tm_g_forest_tte`, `tm_g_ipp`, `tm_g_km`,
  `tm_g_lineplot`, `tm_g_pp_adverse_events`, `tm_g_pp_patient_timeline`,
  `tm_g_pp_therapy`, `tm_g_pp_vitals`, `tm_t_abnormality`,
  `tm_t_abnormality_by_worst_grade`, `tm_t_ancova`,
  `tm_t_binary_outcome`, `tm_t_coxreg`, `tm_t_events`,
  `tm_t_events_by_grade`, `tm_t_events_patyear`, `tm_t_events_summary`,
  `tm_t_exposure`, `tm_t_logistic`, `tm_t_mult_events`,
  `tm_t_pp_basic_info`, `tm_t_pp_laboratory`, `tm_t_pp_medical_history`,
  `tm_t_pp_prior_medication`, `tm_t_shift_by_arm`,
  `tm_t_shift_by_arm_by_worst`, `tm_t_shift_by_grade`, `tm_t_smq`,
  `tm_t_summary`, `tm_t_summary_by`, and `tm_t_tte`

#### Enhancements

- Updated the documentation and vignettes to demonstrate method to pass
  `teal_data` object to
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
- Simplify examples and vignettes code by removing package prefixes
  where possible.
- Added parameter `sort_freq_col` to `tm_t_events` to allow the user to
  select column to use when sorting by decreasing frequency.
- Added parameter `incl_overall_sum` to `tm_t_events` to allow the user
  to choose whether overall summary rows are included at the top of the
  table.
- Updated the documentation and vignettes to demonstrate method to pass
  `teal_data` object to
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
- Added `default_total_label` and `set_default_total_label` functions to
  get and set default total column label (`total_label`) for modules.
- Implemented
  [`tern::default_na_str`](https://insightsengineering.github.io/tern/latest-tag/reference/default_na_str.html)
  and
  [`tern::set_default_na_str`](https://insightsengineering.github.io/tern/latest-tag/reference/default_na_str.html)
  functions to get and set default missing value replacement string
  (`na_level`) for modules.

#### Bug fixes

- Fixed bug in `tm_g_lineplot` forcing module to initialize with a
  table.
- Fixes partial matching.

#### Miscellaneous

- Deprecated the `aval` argument in `tm_t_pp_laboratory` and
  `tm_g_pp_vitals` and replaced it with the `aval_var` argument.
- Deprecated the `avalu` argument in `tm_t_pp_laboratory` and replaced
  it with the `avalu_var` argument.
- Deprecated the `base_var` argument in `tm_g_ipp`, `tm_t_shift_by_arm`,
  and `template_shift_by_arm_by_worst` and replaced it with the
  `baseline_var` argument.
- Specified minimal version of package dependencies.
- Replaced usage of deprecated `summarize_vars` function with
  `analyze_vars`.
- Reduced package dependencies (removed `tidyr`, `rlang`, `magrittr` and
  `styler`).
- Introduced tests against partial matching.

## teal.modules.clinical 0.8.16

#### Enhancements

- Added more informative error message when grade mapping error occurs
  in `tm_t_abnormality_by_worst_grade`.
- Fixed label indentation in `tm_t_abnormality_by_worst_grade`.
- Added `total_label` argument to enable customization of the “All
  Patients” column/row label in the following modules: `tm_a_mmrm`,
  `tm_t_abnormality`, `tm_t_abnormality_by_worst_grade`,
  `tm_t_binary_outcome`, `tm_t_events`, `tm_t_events_by_grade`,
  `tm_t_events_patyear`, `tm_t_events_summary`, `tm_t_exposure`,
  `tm_t_mult_events`, `tm_t_shift_by_arm`, `tm_t_shift_by_arm_worst`,
  `tm_t_shift_by_grade`, `tm_t_smq`, `tm_t_summary`, `tm_t_summary_by`,
  and `tm_t_tte`.
- Increased default width of `tm_g_forest_tte` plot to prevent
  overlapping text.
- Improved default annotation table sizing in `tm_g_km`.
- Refactored `tm_t_exposure` to display “total” row as last row in table
  instead of as a summary row. Added parameters `add_total_row` to set
  whether the total row should be displayed and `total_row_label` to set
  the total row label.
- Updated `tm_t_events` to maintain indentation after pruning.
- Updated default reference/comparison arm level selection to work when
  arm variable levels are filtered out.
- Updated `tm_t_coxreg` to drop factor covariate variable levels that
  are not present to avoid errors when filtering.
- Updated `tm_t_pp_basic_info`, `tm_t_pp_medical_history`,
  `tm_g_pp_therapy`, `tm_g_pp_adverse_events`, and `tm_t_pp_laboratory`
  to print patient ID above table.
- Updated `tm_t_pp_basic_info`, `tm_g_pp_therapy`,
  `tm_g_pp_adverse_events`, and `tm_t_pp_laboratory` to use `rlistings`
  to print data neatly in reports.
- Updated `tm_g_lineplot` to allow user to remove interval from plot.
- Updated the documentation and vignettes to demonstrate method to pass
  `teal_data` object to
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).

#### Bug fixes

- Fixed bug in `tm_t_coxreg` preventing table from being displayed when
  no covariates are selected.

#### Miscellaneous

- Updated `control_incidence_rate` parameter names in
  `tm_t_events_patyear` from `time_unit_input` and `time_unit_output` to
  `input_time_unit` and `num_pt_year`, respectively, after parameter
  names were changed in `tern`.
- Hid the datasets that were not being used by the *patient profile*
  modules in the filter panel. Replaced the `datanames = "all"`
  parameter with the datasets used internally by the module.

## teal.modules.clinical 0.8.15

#### Breaking changes

- Replaced `chunks` with simpler `qenv` class.
- Replaced `datasets` argument containing `FilteredData` with the new
  arguments `data` (`tdata` object) and `filter_panel_api`
  (`FilterPanelAPI`).

#### Enhancements

- Replaced `synthetic_cdisc_data` with refactored
  `synthetic_cdisc_dataset` function to speed up dataset loading in
  tests/examples.
- Added new GEE module `tm_a_gee`.
- Added interface for selecting an interaction term to `tm_t_ancova`.
- Updated encoding input checks to use
  [`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html)
  for better UI experience. Previously used
  [`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).
- Added option to `tm_a_mmrm` to allow for `Kenward-Roger` adjustments
  of standard errors and p-values.
- Added option to choose facet scale options in `tm_g_barchart_simple`.
- Added `label` parameter to `cs_to_select_spec`/`cs_to_des_select` and
  `cs_to_filter_spec`/`cs_to_des_filter` to allow the user to customize
  the label printed over the selection field.
- Updated `tm_t_coxreg` module after refactoring `summarize_coxreg` in
  `tern` to fix indentation.
- Updated `tm_t_exposure` module to use new function
  `analyze_patients_exposure_in_cols` to fix table structure.

#### Bug fixes

- Fixed bug causing overlapping bars in `tm_g_barchart_simple`.
- Fixed bug for figures in `svg` format.
- Fixed bug in `tm_t_summary` and `tm_t_summary_by` preventing users
  from specifying the `numeric_stats` argument.

#### Miscellaneous

- Updated package Suggests to use `scda.2022` rather than `scda.2021`.
- Removed unused argument `param` from `tm_g_pp_vitals`.
- Removed optimizer choice from `tm_a_mmrm` since we can always use the
  automatically determined optimizer.
- Created datasets to use in examples/tests for `adsl`, `adae`,
  `adaette`, `adcm`, `adeg`, `adex`, `adlb`, `admh`, `adqs`, `adrs`,
  `adtte`, and `advs`. These datasets are stored in the data folder and
  accessible via the `tmc_ex_*` prefix.
- Updated all examples and tests to use datasets from the
  `teal.modules.clinical` package instead of `scda` datasets.
- Updated tests to use `testthat` 3rd edition and replaced all
  applicable tests with snapshot testing.
- Implemented the `lubridate` package for date variables in internal
  data.
- Changed default value of `plot_width` in `tm_g_forest_rsp` to prevent
  clutter.

## teal.modules.clinical 0.8.14

#### Enhancements

- Updated all synthetic data for tests to version `rcd_2022_02_28`.
- Updated all test files in `tests/testthat/` to
  `synthetic_cdisc_data("2022_02_28")`
- Reverted missing data checkbox in `tm_t_summary` (encoding and
  filtering should be separate).
- Implemented a new widget that allows dragging and dropping to select
  comparison groups.
- Added the `teal.reporter` functionality to all modules.
- Enhanced the `tm_t_pp_medical_history` module to use the
  `table_with_settings` module and return an `rtables` object.
- Implemented `nestcolor` in examples, refactored `tm_g_barchart_simple`
  to allow use of `nestcolor`.
- Added more descriptive title/labels and visit name subtitle to
  `tm_g_ci`.
- Updated `tm_a_mmrm` column name when deselecting treatment from “all
  obs” to “All Patients”, added subtitles and footnotes.
- Added a title and parameter category subtitle to `tm_t_exposure`,
  cleaned up labels.
- Added titles and worse flag variable subtitles to
  `tm_t_shift_by_grade` and `tm_t_shift_by_arm_by_worst`.
- Added a footnote to `tm_t_events_patyear` with CI method.
- Added a subtitle and footnotes to `tm_g_km`.
- Added Stratified Analysis CI method option panel to
  `tm_t_binary_outcome`.
- Added validation for covariate/visit conflicts to `tm_a_mmrm`.
- Remove unnecessary brackets from header in `tm_t_exposure`.
- Hid footnotes in `tm_g_km` and `tm_t_tte` when “Compare Treatments” is
  off.

#### Bug fixes

- Fixed bug in `tm_g_barchart_simple` which prevented display of graph.
- Fixed broken example for `tm_t_abnormality_by_worst_grade`.
- Fixed bug in `tm_a_mmrm` which prevented table headers from
  displaying.
- Fixed bug in `tm_g_forest_rsp` when deselecting endpoint.
- Fixed bug in `tm_t_binary_outcome` that crashed the app when
  deselecting all `paramcd`.
- Fixed `teal.reporter` card names for `tm_t_smq`.
- Fixed bug in `tm_t_shift_by_arm_by_worst` by adding validations for
  choosing different endpoint values.
- Fixed bug in `tm_t_coxreg` preventing footnotes from displaying for
  univariate models.

#### Miscellaneous

- Added `nestcolor` dependency and replaced deprecated function
  `tern::color_palette` with
  [`nestcolor::color_palette`](https://insightsengineering.github.io/nestcolor/latest-tag/reference/color_palette.html).

## teal.modules.clinical 0.8.13

#### Enhancements

##### `tm_g_pp_adverse_events`

- Updated the position of the labels.
- Updated the plot to render the color legend.

##### `tm_t_summary_by`

- Enhanced the module to support the geometric mean in the encoding
  panel.

##### `tm_t_summary`

- Updated and added a footnote.
- Enhanced the module to support geometric mean in the encoding panel.
- Updated the module to display the checkboxes for numeric variables
  statistics when numeric variables are part of the selected.
- Updated validations to warn users when using a dataset with non unique
  identifiers or when selecting variables with non supported types
  (i.e. `Date`, `POSIXt`).
- Added a checkbox to remove the column generated by missing values.

##### Other modules

- Updated `tm_t_binary_outcome` to enable an option to apply a
  continuity correction in the `Newcombe` method.
- Simplified the show R code for `tm_g_pp_patient_timeline` module.
- Improved the names of the code chunks shown in `Debug Info`.
- Improved the validation if treatment variable is not a factor.

#### Bug fixes

- Fixed `summarize_logistic` implementation broken by empty string
  error. upstream. `_NA_` is the new standard flag to allow it to pivot
  over empty entries in data frames.
- Took out `@title` from `tm_t_binary_outcome.R` that was producing a
  warning.
- Updated validation to account for the error when selecting a single
  variable in `tm_g_pp_patient_timeline` module.

#### Miscellaneous

- Added a `pkgdown` template to the documentation.
- Updated package authors.

## teal.modules.clinical 0.8.12

#### Miscellaneous

- Changed the input of `Covariates` in `tm_t_coxreg.R` to track user
  input and reflect the order in the table.

## teal.modules.clinical 0.8.12

#### New features

- Added new module `tm_t_shift_by_arm_by_worst` for the analysis of the
  laboratory abnormalities with the most severe grade flag.
- Enhanced `tm_t_events_patyear` to include selected parameter in title
  of the table.
- Enhanced `tm_t_mult_events` to include selected parameter in title of
  the table.

#### Enhancements

- Rewrote modules to use `moduleServer` and updated calls to
  `teal.devel` modules which were also written to use `moduleServer`.
- Changed the way of obtaining of selection ordered after changes in
  `teal.devel`. Use `ordered = TRUE` in `cs_to_des_select` or
  `cs_to_select_spec` to return ordered selection.
- Replaced calls to `teal::root_modules` with
  [`teal::modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
  following deprecation of `teal::root_modules`.
- `tm_t_events_summary` now allows nested `arm_var` columns matching
  other outputs such as `tm_t_events`.
- Added validation in `tm_t_abnormality_by_worst_grade` when `arm_var`
  is not selected.
- Enhanced `tm_t_binary_outcome` to include all responders in the
  response table by default.
- Added a subtitle to `tm_g_forest_tte`, `tm_t_coxreg`, and
  `tm_t_binary_outcome` listing stratification factors.

#### Bug fixes

- Fixed a bug to prevent processing of empty sets of data by
  `tm_g_forest_rsp.R` causing `shiny` errors during the runtime.
- Fixed a bug where a closed `Compare Treatments` conditional panel with
  a marked `Combine all comparison groups?` option conflicted with
  adding a column with all patients to tables in `tm_t_binary_outcome.R`
  and `tm_t_tte.R`.

#### Miscellaneous

- Replaced the deprecated `rtables::var_labels` calls in the
  documentation examples.
- Add import of `tern.mmrm` package and change some references after
  split of `tern`.
- Adjusted package imports to take into account changes to the `teal`
  framework.
- Ensure consistent vertical order of `tm_g_pp_patient_timeline` output
  when switching between absolute and relative days.

## teal.modules.clinical 0.8.11

#### Breaking changes

- Updated `tm_t_abnormality` due to changes in `count_abnormal` that
  `abnormal` argument is taking list as input now.
- Changed the `tm_g_pp_patient_timeline` parameter, `cmtrt`, to
  `cmdecod`.

#### New features

- Added new module `tm_t_abnormality_by_worst_grade` for the analysis of
  laboratory test results with highest grade post-baseline.
- Enhanced `tm_t_ancova` to include selected parameter(s), visit(s) and
  the analysis variable in title of the table.
- Added new module `tm_g_lineplot` for creating line plots.
- Enhanced `tm_t_logistic` to include selected parameter in title of the
  table.
- Enhanced `tm_g_forest_rsp` to include selected parameter in title of
  the table.
- Enhanced `tm_g_forest_tte` to include selected parameter in title of
  the table.
- Enhanced `tm_g_pp_patient_timeline` with bold axes labels and integer
  values on the axis.
- Enhanced `tm_g_ipp` to allow users not to display `AVALU` in the title
  nor in the y axis.

#### Enhancements

- Added support for logging with the `logger` package and added info
  level logs upon initialization of a module.
- Added `default_responses` argument to `tm_t_binary_outcome` and
  `tm_g_forest_rsp` to allow the user to specify default selected
  responses and possible response levels.
- Updated `tm_t_binary_outcome` to show selected responses in the output
  table when selecting “Show All Selected Response Categories”.
- Added `rsp_table` argument to `tm_t_binary_outcome` to allow the user
  to initialize the module matching the `RSPT01` STREAM template.
- Added support for custom arguments for
  [`ggplot2::labs`](https://ggplot2.tidyverse.org/reference/labs.html)
  and
  [`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html)
  in plot based modules.
- Added support for custom arguments for
  [`rtables::basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)
  in table based modules.
- Updated `tm_t_binary_outcome` to enable an option to apply a
  continuity correction in the Wilson method.

#### Miscellaneous

- Updated required R version to \>= 3.6.
- Refactored calls to the defunct `teal.devel::data_extract_input` into
  calls to its replacement `teal.devel::data_extract_ui`.
- Updated modules to use new `data_merge_module` interface provided by
  `teal.devel` and removed usage of the now deprecated function
  `teal.devel::get_input_order`.
- Updated `tm_t_binary_outcome` module to add a template and removed the
  now deprecated module `tm_t_rsp`.
- Removed `utils.nest` dependency and replaced calls with `checkmate`
  equivalents.

#### Bug Fixes

- Fixed bug in `tm_g_pp_therapy` where if the `cmstdy` or `cmendy`
  argument is of type `integer` causes the plot to crash.

## teal.modules.clinical 0.8.10

#### New features

- Added new module `tm_t_smq` for the analysis of adverse events by
  Standardized `MedDRA` Query.
- Added new module `tm_t_shift_by_grade` for the analysis of grade
  laboratory abnormalities.
- Added new module `tm_t_exposure` for the analysis of duration of
  exposure for risk management plan.
- Added new module `tm_t_shift_by_arm` that can display shift table of
  ECG interval data.

#### Bug fixes

- Corrected `tm_a_mmrm` to be able to consider the treatment variable in
  all interactions.
- Fixed `tm_t_binary_outcome` and `tm_t_rsp` to choose the correct CI
  estimation method for Proportions Difference in Stratified Analysis
  (i.e. Wald-type confidence interval with `CMH` weights).

#### Enhancements

- Added validation checks to `tm_t_rsp` and `tm_t_binary_outcome` for
  stratification errors from applied filters.
- Added `tm_g_km` validation check for plot tables font size.
- Enhanced `tm_g_km` to add the selected `paramcd` in the plot title.
- `tm_t_events` now can display layouts with two nested column as
  treatment variables. The same options for pruning and sorting are
  available.
- Exported package helper functions.
- Updated `tm_t_events_by_grade` to display grading groups in nested
  columns with `col_by_grade` option and support pruning and sorting
  options like `tm_t_events`.
- Used `format_count_fraction` to fix formatting inconsistency in
  `tm_t_events_summary`.
- Updated `count_occurrences` `vars` argument in `tm_t_shift_by_grade`.
- Updated `tm_t_pp_laboratory` to display 4 decimals by default.
- Updated `tm_t_events_by_grade` to use the `trim_levels_in_group` split
  function instead of the `trim_rows` function.
- Added a table title to `tm_t_tte`.
- Added table titles to `tm_t_rsp` and `tm_t_binary_outcome`.

#### Miscellaneous

- Updated `LICENCE` and `README` with new package references.
- Added `error_on_lint: TRUE` to `.lintr`.
- Removed `insert_rrow` and updated usage of `count_patients_by_flags`
  in `tm_t_events_summary`.
- Changed how the package calls functions from the `dplyr` package. The
  functions should be now fully specified
  (e.g. [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)).

## teal.modules.clinical 0.8.9

#### New features

- Added capability to remember the order of user input to some encoding
  UI elements. Inputs marked with a double arrow icon have tracking
  enabled. The affected modules are: `tm_t_summary`, `tm_t_summary_by`,
  `tm_g_forest_rsp`, `tm_g_forest_tte`, `tm_t_events_summary`,
  `tm_t_abnormality`, `tm_t_mult_events`.
- Added a new argument `numeric_stats` to `tm_t_summary` and
  `tm_t_summary_by` to control displayed summary statistics for numeric
  variables.
- Added a new argument `drop_zero_levels` to `tm_t_summary_by` so that
  you can drop rows with all zeros from result table.

#### Enhancements

- Split `tm_g_patient_profile` tabs into 8 separate new modules.
- Added the option to select patient ID from the filter panel for all
  the modules of patient profile.
- Added a validation for `tm_g_patient_timeline` when the plot is empty.
- Enhanced `tm_a_mmrm` to work without the treatment variable.
- Added the option to choose the number of decimal places for rounding
  in `tm_t_pp_laboratory`.
- Added a check box to `tm_g_pp_patient_timeline` hiding/showing
  relative study days on the x-axis.
- Added a title with patient’s id to plots in patient profile modules.
- Made the gray error message in `tm_g_forest_tte` more informative when
  deselecting the `Endpoint` column in the left-hand encoding panel.
- Added the twenty-fifth and seventy-fifth quantile to summary
  statistics in `tm_t_summary`.
- Added an interaction p-value column for `tm_t_coxreg`.
- Added a validation for `tm_t_ancova` when selected covariate variables
  contain one level.
- Added a validation for `tm_t_events_patyear` when the events variable
  is empty.
- Changed the font size input description for `tm_g_km` to more
  precisely describe what it controls.
- Enhanced `tm_t_logistic` so that interaction choices depend on the
  selected covariates.
- Enhanced `tm_t_rsp` so that strata input is visible when comparing
  treatments.

#### Bug fixes

- Fixed Get R Code output of `tm_t_pp_laboratory` to return identical
  HTML formatted table as displayed in the app.
- Added a validation for `tm_t_coxreg` to ensure treatment, strata and
  covariate variables do not overlap.
- Limited the label repel feature in `tm_g_pp_patient_timeline` to
  X-axis for a more consistent look.
- Updated `tm_t_summary_by` so that `paramcd` is not required when
  analyzing `ADSL` variables.
- Updated `tm_t_coxreg` so that it can work when there is no covariate
  selected.
- Updated `tm_a_mmrm` so that it can work when treatment variable is not
  selected.
- Updated `tm_g_forest_tte` so that total number of events are also
  shown in the table.
- Updated `tm_t_events_summary` to work with pooled studies.
- Updated validation for the `at` level of `tm_t_coxreg`.
- Updated validation for the `at` level of `tm_t_logistic`.
- Added a validation for `tm_t_binary_outcome` and `tm_t_rsp` to ensure
  strata variable contains more than one level when selecting one strata
  variable.
- Updated the warning message when deselecting all statistics in
  `tm_t_summary` and `tm_t_summary_by` to explain need to select at
  least one statistic.

## teal.modules.clinical 0.8.8

#### Enhancements

- Added the option to download and expand tables.
- In `tm_g_km` added support for downloading images and updated x-axis
  label to show in title case.
- For `tm_g_patient_profile`:
  - Added a slider for the font size in plots.
  - Added persistence for selected table lengths.
  - The timeline plot now supports more edge cases.
  - In vitals tab, removed unused label text legend, updated plot to
    display stable colors per levels, cleared x-axis limit and fixed
    legend to update when filtering. Also added a note to clarify the
    supported horizontal lines cases.
  - Updated adverse events tab to show a warning message instead of an
    empty plot when data is empty.
  - Fixed `PARAMCD` selected levels for current patient.
- For `tm_t_tte`:
  - Updated the pre-processing code inside `template_tte` so that a
    dataset without any events still produces a table.
  - Updated code to use correct denominator for duration of response
    endpoints.
- For `tm_t_summary`:
  - Modified the parameter `arm_var` to accept more than one column.
    When selecting two columns for `arm_var`, the second variable will
    nest under the first one.
  - Added argument `show_labels` to `template_summary` to show the label
    for a single summary variable in the table.
- Added a new parameter `conf_arg` to `tm_t_rsp` to be consistent with
  other efficacy modules.
- Added validation statement in `tm_g_ipp` module to print message when
  deselecting `Timepoint Variable` drop-down.
- Removed header definition in `tm_g_forest_rsp` and `tm_g_forest_tte`
  as there is now a default header in `g_forest`.
- Fixed validation statement in `tm_t_coxreg` so that models without
  strata and using likelihood tests return a result.
- Clarified functionality of `drop_arm_levels` for `tm_t_summary` and
  `tm_t_summary_by`. In the encodings panel, the checkbox will show when
  the parent dataset and analysis dataset are different.

#### Miscellaneous

- Replaced the remaining two `observe` function calls with
  `observeEvent` to optimize performance.
- Fixed grammar in the “Select a patient’s id” error message in the
  `tm_g_patient_profile`.
- Fixed the `font_size` default of the templates to be 12L instead of a
  vector of 3 integers and cleaned associated unnecessary code.
- Fixed deprecated function warning in `tm_g_barchart_simple`.
- Fixed `subgroup_var` definition truncation in `tm_g_forest_rsp` and
  `tm_g_forest_tte`.
- Clarified labeling related to regression type in the encoding panel
  from `tm_t_coxreg`.

#### Bug fixes

- Added a validation for the case when filtering out all rows in the
  therapy tab of `tm_g_patient_profile`.
- Updated the internals of the modules to read data from the correct
  field of the `filter_spec` objects.
- Fixed the reactivity between the filter panel and the `PARAMCD`
  variable levels input in `tm_g_patient_profile` vitals tab so that the
  plot does not get reset when filtering.
- Updated the vitals plot tab in `tm_g_patient_profile` to drop NA
  entries in the plot.
- Updated `tm_t_coxreg` to take at values into account.
- Added a check in `tm_t_coxreg` to have interactions in univariate
  models but not in multivariate models.
- Updated `tm_t_events_summary` to work with pooled studies.

## teal.modules.clinical 0.8.7

#### New Module

- Added new module `tm_g_patient_profile` to profile patients based on
  predefined categories.
- Added new module `tm_g_ipp` for individual patient plots.

#### Enhancements

- Added the argument `drop_arm_levels` to all safety modules. This
  allows removal of columns based on factor levels not found in filtered
  data.
- Updated `tm_g_km` to allow plot of failure probability on y-axis, tick
  interval selection on x-axis and option to create plot without
  confidence interval ribbon (new default).
- Added the argument `time_unit_var` to `template_g_km` to add the time
  unit to the x-axis label.

#### Miscellaneous

- Removed redundant `Analysis Data:` label from Encodings Panel.
- Removed limit requiring 15 or fewer columns for tabulation modules.
  New upper threshold is 100 columns.
- Decreased the lower limit for number of observations required by
  modules. Safety tables require at least one record. Requirements for
  efficacy outputs per treatment group: `tm_a_mmrm` requires five
  records, `tm_t_logistic` and `tm_t_coxreg` require two records, and
  the remaining modules require at least one record per treatment group.
  For graphs, the lower threshold is two records.
- Removed argument `cnsr_val` from `tm_t_events_patyear` and added new
  argument `events_var`.
- `arm_ref_comp_observer` to include `parentname` argument.
- Show R code to include datasets retrieved from `data_extract_spec`
  objects.
- Refactored out the `stringr` dependency from the patient profile
  module.
- Added missing table calls in chunks for `tm_t_events` and
  `tm_t_events_by_grade`.

## teal.modules.clinical 0.8.6

#### New Features

- Added new module `tm_g_ci` for confidence interval plots.
- Added new module `tm_t_ancova` for analysis of variance summary
  tables.
- Added new module `tm_t_mult_events` for multi-event tables.

#### Enhancements

- Refactored all modules using the redesigned `rtables` and `tern`
  packages.
- Enhanced modules. They now take advantage of `data_extract_spec` and
  `data_merge_module` functionality from `teal`.
- Reduced clutter from repeated datasets in the encodings panels.
- Updated all modules to use `OptionalSelectInput` for `conf_level`.

#### Miscellaneous

- Added vignette about `substitute` which can be helpful when developing
  analysis template functions for teal modules.

#### Bug fixes

- Updated `tm_t_events` module to use user’s non-default choices for
  `prune_freq` and `prune_diff`.

## teal.modules.clinical 0.8.5

- All graph modules now accept a `plot_width` argument which specifies
  the plot width and renders a slider to adjust the width interactively
  in the module.
- `FilteredData` object is now passed to `arm_ref_comp_observer` and
  modules now support nested lists containing `delayed_data` objects.
- Replace `plot_with_height` module with new `plot_with_settings`
  module.
- Update examples to use `code` argument inside `cdisc_dataset`.

## teal.modules.clinical 0.8.4

- Extend `tm_t_coxreg` to optionally produce univariate Cox regressions.
- Updated `tm_t_binary_outcome` to display Odds Ratio estimates, include
  new methods for CIs and p-values and display a summary for individual
  response categories.
- Updated `tm_t_tte` to optionally compare between arms, removed
  `conf_level` argument.
- Updated `tm_g_km` to optionally compare between arms.
- Extend `tm_g_km` to optionally scale X axis range in case of more than
  one plot.
- New `tm_a_mmrm` for `MMRM` analysis.
- Deprecated `tm_t_mmrm` (superseded by `tm_a_mmrm`).

## teal.modules.clinical 0.8.3

- New `tm_t_coxreg` module for multi-variable Cox regressions.
- New `tm_t_binary_outcome` module.
- New `tm_t_events_patyear` module: events rate adjusted for
  patient-year at risk table.
- Remove `grade_levels` argument from `tm_t_events_by_grade`.
- Updated response table for single arm.
- New `tm_t_abnormality` module.
- Removed `get_relabel_call` and `get_relabel_call2` in favor of
  `teal.devel::get_relabel_call` and `teal.devel::get_anl_relabel_call`.

## teal.modules.clinical 0.8.2

- Add confidence level for `survfit`, `coxph`, `ztest`; add confidence
  type, ties, percentiles to `tm_t_tte`.
- Optionally use a single term in `tm_t_events` and
  `tm_t_events_by_grade` modules.
- New `tm_t_logistic` module.
- New `tm_t_mmrm` module.
- New modules `tm_t_summary_by` and `tm_t_events_summary`.
- Add stratified analysis to `tm_g_forest_tte` and `tm_g_forest_rsp`.
- Add confidence level and plotting symbol size options to
  `tm_g_forest_rsp` and `tm_g_forest_tte`.

## teal.modules.clinical 0.8.1

- New `tm_t_events` and `tm_t_events_by_grade` modules.

## teal.modules.clinical 0.8.0

- Optionally show KM and `CoxPH` table in `tm_g_km`.

## teal.modules.clinical 0.7.0

- Use `teal.devel`.

## teal.modules.clinical 0.6.0

- Package renamed to `teal.modules.clinical`.
- Rename `tm_t_summarize_variables` to `tm_t_summary`.
- Usage of `teal::choices_selected()` function instead of `*_var` and
  `*_var_choices` arguments.
