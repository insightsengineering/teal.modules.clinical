# teal.modules.clinical 0.8.8
### Enhancements
* Added a slider for the font size in the plots in `tm_g_patient_profile`.
* Updated the pre-processing code for `template_tte` so that a case with no events still produces a table.
* Added persistence for selected table lengths in `tm_g_patient_profile`.
* Added a new parameter `conf_arg` to `tm_t_rsp` to be consistent with other efficacy modules.
* The timeline plot inside `tm_patient_profile` is supporting more edge cases.
* Added the option to download and expand tables.
* Fixed subgroup_var definition truncation in `tm_g_forest_rsp` and `tm_g_forest_tte`. 
* Added support for downloading images in `tm_g_km`.
* Added the parameter `show_labels` to the `template_summary` function for single summarize variable to explicitly show in resulting table.
* Modified the parameter `arm_var` in `tm_t_summary` to accept more than one column. If two columns were selected for `arm_var`, then the second variable will be nested under the first one.
* Updated `xlab` in title case in `tm_g_km`.
* Added the argument `show_labels` to `template_summary` function for single summarize variable to explicitly show in resulting table.
* Added validate statement in `tm_g_ipp` module to print message when `Timepoint Variable` drop down is deselected.
* Clarified "Type of Regression" related labeling in the encoding panel from `tm_t_coxreg`.
* Removed the header in `tm_g_forest_rsp` and `tm_g_forest_tte` as there's a default header in `g_forest`.

### Miscellaneous
* Replaced the remaining two `observe` function calls with `observeEvent` to optimize performance.
* Fixed grammar in the "Select a patient's id" error message in the `tm_g_patient_profile`.
* Fixed the `font_size` default of the templates to be 12L instead of a vector of 3 integers and cleaned associated unnecessary code.

### Bug fixes
* Added a validation for the case when all rows are filtered out in the therapy tab of `tm_g_patient_profile`.

# teal.modules.clinical 0.8.7
### New Module
* New module `tm_g_patient_profile` was added to profile patients based on predefined categories.
* New module `tm_g_ipp` was added for individual patient plots.

### Enhancements
* The argument `drop_arm_levels` was added to all safety modules. This allows removal of columns based on factor levels not found in filtered data.
* `tm_g_km` updated to allow plot of failure probability on y-axis, tick interval selection on x-axis and option to create plot without confidence interval ribbon (new default).
* The argument `time_unit_var` was added to `template_g_km` to add the time unit to the x-axis label.

### Miscellaneous
* Removed redundant `Analysis Data:` label from Encodings Panel.
* Removed limit requiring 15 or fewer columns for tabulation modules. New maximum threshold is 100 columns.
* Lowered limit for minimum number of observations required by modules. Safety tables require at least one record. For efficacy outputs the requirement is per treatment group: `tm_a_mmrm` requires five records, `tm_t_logistic` and `tm_t_coxreg` require two records and the remaining modules require at least one record per treatment group. For graphs, the minimum threshold is two records. 
* Removed argument `cnsr_val` from `tm_t_events_patyear` and added new argument `events_var`.
* `arm_ref_comp_observer` to include `parentname` argument.
* Show R code to include datasets retrieved from `data_extract_spec` objects.
* Refactored out the `stringr` dependency from the patient profile module.
* Added missing table calls in chunks for `tm_t_events` and `tm_t_events_by_grade`.

# teal.modules.clinical 0.8.6

### New Features
* New module `tm_g_ci` was added for confidence interval plots.
* New module `tm_t_ancova` was added for analysis of variance summary tables.
* New module `tm_t_mult_events` was added for multiple events by term.

### Enhancements
* Refactored all modules using the redesigned `rtables` and `tern` packages.
* Enhanced modules. They now take advantage of `data_extract_spec` and `data_merge_module` functionality from `teal`.
* Reduced clutter from repeated datasets in the encodings panels.
* Updated all modules to use `OptionalSelectInput` for `conf_level`.

### Miscellaneous
* Added vignette about `substitute` which can be helpful when developing analysis template functions for teal modules.

### Bug fixes
* Updated `tm_t_events` module to use user's choices for `prune_freq` and `prune_diff` when non-default is used

# teal.modules.clinical 0.8.5

* All graph modules now accept a `plot_width` argument which specifies the plot width and renders a slider to adjust the width interactively in the module.
* `FilteredData` object is now passed to `arm_ref_comp_observer` and modules now support nested lists containing `delayed_data` objects.
* Replace `plot_with_height` module with new `plot_with_settings` module.
* Update examples to use `code` argument inside `cdisc_dataset`.

# teal.modules.clinical 0.8.4

* Extend `tm_t_coxreg` to optionally produce univariate Cox regressions.
* Updated `tm_t_binary_outcome` to display Odds Ratio estimates, include new methods for CIs and p-values and 
display a summary for individual response categories.
* Updated `tm_t_tte` to optionally compare between arms, removed `conf_level` argument.
* Updated `tm_g_km` to optionally compare between arms.
* Extend `tm_g_km` to optionally scale X axis range in case of multiple plots.
* New `tm_a_mmrm` for MMRM analysis.
* Deprecated `tm_t_mmrm` (superseded by `tm_a_mmrm`).

# teal.modules.clinical 0.8.3

* New `tm_t_coxreg` module for multi-variable Cox regressions.
* New `tm_t_binary_outcome` module.
* New `tm_t_events_patyear` module: events rate adjusted for patient-year at risk table.
* Remove `grade_levels` argument from `tm_t_events_by_grade`.
* Response table is updated for single arm.
* New `tm_t_abnormality` module.
* Removed `get_relabel_call` and `get_relabel_call2` in favor of `teal.devel::get_relabel_call` and `teal.devel::get_anl_relabel_call`.

# teal.modules.clinical 0.8.2

* Add confidence level for `survfit`, `coxph`, `ztest`; add confidence type, ties, percentiles to `tm_t_tte`.
* Optionally use only single term in `tm_t_events` and `tm_t_events_by_grade` modules.
* New `tm_t_logistic` module.
* New `tm_t_mmrm` module.
* New modules `tm_t_summary_by` and `tm_t_events_summary`.
* Add stratified analysis to `tm_g_forest_tte` and `tm_g_forest_rsp`.
* Add confidence level and plotting symbol size options to `tm_g_forest_rsp` and `tm_g_forest_tte`.

# teal.modules.clinical 0.8.1

* New `tm_t_events` and `tm_t_events_by_grade` modules.

# teal.modules.clinical 0.8.0

* Optionally show KM and CoxPH table in `tm_g_km`.

# teal.modules.clinical 0.7.0

* Use `teal.devel`.

# teal.modules.clinical 0.6.0

* Package renamed to `teal.modules.clinical`.
* Rename `tm_t_summarize_variables` to `tm_t_summary`.
* Usage of `teal::choices_selected()` function instead of `*_var` and `*_var_choices` type of arguments.

# teal.tern 0.5.0.2

* Tables displayed with horizontal scrollbar if needed.

# teal.tern 0.5.0

* Package renamed to `teal.tern`.
* Teal modules are now named according to the function names in `tern` prefixed with an `tm_`.
* Harmonized argument names.

# teal.oncology 0.0.2

* `teal.oncology` was split into two packages: `tern` and `teal.oncology` where `tern` contains all the analysis code and `teal.oncology` contains the the teal modules that make the analysis function interactive with encodings and filtering. The `tern` package can be found here: [https://github.roche.com/NEST/tern](https://github.roche.com/NEST/tern).

# teal.oncology 0.0.1

* Initial version used for the first two atezo SREPs.
