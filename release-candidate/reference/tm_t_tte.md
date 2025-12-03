# teal Module: Time-To-Event Table

This module produces a time-to-event analysis summary table, consistent
with the TLG Catalog template for `TTET01` available
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/ttet01.html).

## Usage

``` r
tm_t_tte(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  arm_ref_comp = NULL,
  paramcd,
  strata_var,
  aval_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVAL"), "AVAL", fixed = TRUE),
  cnsr_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "CNSR"), "CNSR", fixed = TRUE),
  conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order
    = TRUE),
  conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
    keep_order = TRUE),
  time_points,
  time_unit_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, "AVALU"),
    "AVALU", fixed = TRUE),
  event_desc_var = teal.transform::choices_selected("EVNTDESC", "EVNTDESC", fixed = TRUE),
  add_total = FALSE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character`)  
  menu item label of the module in the teal app.

- dataname:

  (`character`)  
  analysis data used in teal module.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- arm_ref_comp:

  (`list`) optional,  
  if specified it must be a named list with each element corresponding
  to an arm variable in `ADSL` and the element must be another list
  (possibly with delayed
  [`teal.transform::variable_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)
  or delayed
  [`teal.transform::value_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/value_choices.html)
  with the elements named `ref` and `comp` that the defined the default
  reference and comparison arms when the arm variable is changed.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- strata_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  names of the variables for stratified analysis.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- cnsr_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  censoring variable.

- conf_level_coxph:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for
  confidence level, each within range of (0, 1).

- conf_level_survfit:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for
  confidence level, each within range of (0, 1).

- time_points:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for time
  points that can be used in
  [`tern::surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html).

- time_unit_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the time
  unit variable.

- event_desc_var:

  (`character` or
  [`teal.transform::data_extract_spec()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html))  
  variable name with the event description information, optional.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- pre_output:

  (`shiny.tag`) optional,  
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional,  
  with text placed after the output to put the output into context. For
  example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- basic_table_args:

  (`basic_table_args`) optional  
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of lists of
  `teal_transform_module`) optional, decorator for tables or plots
  included in the module output reported. The decorators are applied to
  the respective output objects.

  See section "Decorating Module" below for more details.

## Value

a `teal_module` object.

## Details

- The core functionality of this module is based on
  [`tern::coxph_pairwise()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_coxph_pairwise.html),
  [`tern::surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html),
  and
  [`tern::surv_time()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_time.html)
  from the `tern` package.

- The arm and stratification variables are taken from the `parentname`
  data.

- The following variables are used in the module:

  - `AVAL`: time to event

  - `CNSR`: 1 if record in `AVAL` is censored, 0 otherwise

  - `PARAMCD`: variable used to filter for endpoint (e.g. OS). After
    filtering for `PARAMCD` one observation per patient is expected

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_tte(
       ..., # arguments for module
       decorators = list(
         table = teal_transform_module(...) # applied only to `table` output
       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.modules.clinical")`](https://insightsengineering.github.io/teal.modules.clinical/articles/decorate-module-output.md).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## See also

The [TLG
Catalog](https://insightsengineering.github.io/tlg-catalog/stable/)
where additional example apps implementing this module can be found.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOzbWycikrLK6v5SciUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XCByCoqTgYAUwa-zHs4urppbb0PvHnOaXGA3pcWDA-IwlusiDBUEU6KJSOwGpkAMK2TJYACyKOyOgAvDp4YiGhkIcZ8To-lidAAhR64Ek6YgwikEJFPamZemUjmYnQox6DDJyBkQDIYvkEolIsUZHRkimPGmIHQABWoUAIcHoRHpjOZsIJbL+KuyjEEAHMdAANbmPFEqlHQ+jxDyWCCChqDQZKNCw8LxaIyjJJClJUUZPhCESiClR4RiYNykp+NY9OBJuWEqD0ODURVgWy0eB2Ig6JzaMh2HMiPWyrNJI66Al-T6AiNZnSgvyaFismKjLWiPyiPNwdRwfiZzu9xi0GtwdYDmSJ54pY28u28nFb1HorE7sByEWMrNUzGPU86E-1uXdslQlkE++Qg0drOoFiwDb9wdiEdjhOU5XhkvbUIIi4EMuQ7sP8LQpI8qoYpk2LZFuSFYChgrvp2jwAPK5Jet7CjhGQIswng9n2RrQf+o4iEB05ZrO870CIS5-jMa5MuyuROLaeA8jSmJYAA0lgABMgo3p2cqPHxAlXjJnYaPAfioKMZCxjRnEAQx5BTmyACMAAcEkpBJAAsADMIo6KZEnKVmcCVmsizbFRjC-iuw70eOBlMXKLELhxPmwW2CFgE4ABqABytjZE4uQCkepFyVFcUJUlKVpRkxi0BUk4UvYzhKYyQrXkogy0OS7DxOQagaNo1g2GksqiHEECsJk6DsH6AAkgi0CkA2jow2iMIMfRKGAfRnEAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADTTE <- tmc_ex_adtte
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADTTE <- data[["ADTTE"]]

arm_ref_comp <- list(
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  ),
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ADTTE",
      arm_var = choices_selected(
        variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(ADTTE, "PARAMCD", "PARAM"),
        "OS"
      ),
      strata_var = choices_selected(
        variable_choices(ADSL, c("SEX", "BMRKR2")),
        "SEX"
      ),
      time_points = choices_selected(c(182, 243), 182),
      event_desc_var = choices_selected(
        variable_choices(ADTTE, "EVNTDESC"),
        "EVNTDESC",
        fixed = TRUE
      )
    )
  )
)
#> Initializing tm_t_tte
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
