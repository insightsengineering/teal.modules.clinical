# teal.modules.clinical 0.8.3

* new `tm_t_coxreg` module for multi-variable Cox regressions
* new `tm_t_binary_outcome` module
* new `tm_t_events_patyear` module: events rate adjusted for patient-year at risk table
* remove `grade_levels` argument from `tm_t_events_by_grade`
* response table is updated for single arm 
* new `tm_t_abnormality` module

* removed `get_relabel_call` and `get_relabel_call2` in favor of `teal.devel::get_relabel_call` and
`teal.devel::get_anl_relabel_call`.

# teal.modules.clinical 0.8.2

* add confidence level for `survfit`, `coxph`, `ztest`; add confidence type, ties, percentiles to `tm_t_tte`

* optionally use only single term in `tm_t_events` and `tm_t_events_by_grade` modules

* new `tm_t_logistic` module

* new `tm_t_mmrm` module

* new modules `tm_t_summary_by` and `tm_t_events_summary`

* add stratified analysis to `tm_g_forest_tte` and `tm_g_forest_rsp`

* add confidence level and plotting symbol size options to `tm_g_forest_rsp` and `tm_g_forest_tte`

# teal.modules.clinical 0.8.1

* new `tm_t_events` and `tm_t_events_by_grade` modules

# teal.modules.clinical 0.8.0

* optionally show KM and CoxPH table in `tm_g_km`

# teal.modules.clinical 0.7.0

* use `teal.devel`

# teal.modules.clinical 0.6.0

## Changes

* package renamed to `teal.modules.clinical`
* rename `tm_t_summarize_variables` to `tm_t_summary`
* usage of `teal::choices_selected()` function instead of `*_var` and `*_var_choices` type of arguments

# teal.tern 0.5.0.2

## Changes

* tables displayed with horizontal scrollbar if needed


# teal.tern 0.5.0

## Changes
* package renamed to `teal.tern`
* teal modules are now named according to the function names in `tern` prefixed
with an `tm_`
* harmonized argument names

# teal.oncology 0.0.2

## Changes

 * `teal.oncology` was split into two packages: `tern` and `teal.oncology` where
 `tern` contains all the analysis code and `teal.oncology` contains the the teal
 modules that make the analysis function interactive with encodings and
 filtering. The `tern` package can be found here:
 [https://github.roche.com/NEST/tern](https://github.roche.com/NEST/tern).


# teal.oncology 0.0.1

Initial version used for the first two atezo SREPs 
