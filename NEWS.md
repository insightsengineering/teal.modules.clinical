# teal.modules.clinical 0.8.3.9000

* Extend `tm_t_coxreg` to optionally produce univariate Cox regressions.
* Updated `tm_t_binary_outcome` to display Odds Ratio estimates and include new methods for CIs and p-values.

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
