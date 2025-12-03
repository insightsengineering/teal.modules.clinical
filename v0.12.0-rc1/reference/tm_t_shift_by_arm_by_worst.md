# teal Module: Shift by Arm by Worst Analysis Indicator Level

This module produces a summary table of worst analysis indicator
variable level per subject by arm.

## Usage

``` r
tm_t_shift_by_arm_by_worst(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  paramcd,
  aval_var,
  base_var = lifecycle::deprecated(),
  baseline_var,
  worst_flag_var,
  worst_flag,
  treatment_flag_var = teal.transform::choices_selected(choices =
    teal.transform::variable_choices(dataname, subset = "ONTRTFL"), selected = "ONTRTFL"),
  treatment_flag = teal.transform::choices_selected("Y"),
  useNA = c("ifany", "no"),
  na_level = tern::default_na_str(),
  add_total = FALSE,
  total_label = default_total_label(),
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

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- base_var:

  **\[deprecated\]** Please use the `baseline_var` argument instead.

- baseline_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  values that can be used as `baseline_var`.

- worst_flag_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as worst flag variable.

- worst_flag:

  (`character`)  
  value indicating worst analysis indicator level.

- treatment_flag_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  on treatment flag variable.

- treatment_flag:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  value indicating on treatment records in `treatment_flag_var`.

- useNA:

  (`character`)  
  whether missing data (`NA`) should be displayed as a level.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- add_total:

  (`logical`)  
  whether to include row with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

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

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_shift_by_arm_by_worst(
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

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOynAHEikrLK6v44AHMlAF9FCAArIni-AGs4VlFE0Jtw7uMoYVI-An5aUTLR8amZ4Gh4WaS5AF1XCByCoqTgYAUwa-zHs4urptbF0PvHnJbXu8lGhUEV4tF2A0kjoALw6JK4Bp8IQiUSwnTI4RiSEQDIZEp+NaiOLGNb0VjVRgwPzkvwRIiMUSkHF4vHUKD0ODUdGPXIk0g6clZKl2DkiR6I3Gs+GhI66OF-T4ShqsljUzQsdEEGJjAhiPyiLlwdRwfgs6UZDWMWhiuDrHUybHPFKiQT0Q0CuEESFPLAAWQlOj+-oAwtlHnI5JKLRlDSITfweb6A2AVXio2mMqgWLANlqHXrRAajQnzRaNdRBHbtbqnZ8Uo8AAqZLCZP1hiPRmNx43kRMKsBOEPNACSADlbI3HpmdBmpaz6Yy1sZ2T0-Fb87Wiz3SzPLSwbfQRPat+x-s0Ut7HgB1ADyWFyAAYAEwAMReeCDYDvD8fAEZ3wjOcYx0Hc+yTH8nzfD8Z2A6VFyZPwVygHpN0dbcSz7MtpQrKsT3Qs96y-SCX0AsA4ItMDTSTABNZV52lYxaAqai4XsZxYK7VUK3XTUvQLfUqLNPcdCtQ9jxrAjz0vH1MgANUyfIQ0DP4xywcdw3IijpSEpN5MU5TUwYjJtIyegoDjeI7Q3fit2LeMsJEsTbXwwtCJaGTHgAIUyXIhxUsAvLUjSgK4nTMNYr8fL8wzOJnQRDTHTIk1oZYIFYacGKGEylCGVKdHYeJyDUDRtGsGw0ilYl4lYTJ0HYEEABJBFoFImsNRhtEYIZ+iUMB+jOIA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADEG <- tmc_ex_adeg
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADEG <- data[["ADEG"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_shift_by_arm_by_worst(
      label = "Shift by Arm Table",
      dataname = "ADEG",
      arm_var = choices_selected(
        variable_choices(ADSL, subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      paramcd = choices_selected(
        value_choices(ADEG, "PARAMCD"),
        selected = "ECGINTP"
      ),
      worst_flag_var = choices_selected(
        variable_choices(ADEG, c("WORS02FL", "WORS01FL")),
        selected = "WORS02FL"
      ),
      worst_flag = choices_selected(
        value_choices(ADEG, "WORS02FL"),
        selected = "Y",
        fixed = TRUE
      ),
      aval_var = choices_selected(
        variable_choices(ADEG, c("AVALC", "ANRIND")),
        selected = "AVALC"
      ),
      baseline_var = choices_selected(
        variable_choices(ADEG, c("BASEC", "BNRIND")),
        selected = "BASEC"
      ),
      useNA = "ifany"
    )
  )
)
#> Initializing tm_t_shift_by_arm_by_worst
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
