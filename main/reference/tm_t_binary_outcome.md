# teal Module: Binary Outcome Table

This module produces a binary outcome response summary table, with the
option to match the template for response table `RSPT01` available in
the TLG Catalog
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/rspt01.html).

## Usage

``` r
tm_t_binary_outcome(
  label,
  dataname,
  parentname = ifelse(test = inherits(arm_var, "data_extract_spec"), yes =
    teal.transform::datanames_input(arm_var), no = "ADSL"),
  arm_var,
  arm_ref_comp = NULL,
  paramcd,
  strata_var,
  aval_var = teal.transform::choices_selected(choices =
    teal.transform::variable_choices(dataname, c("AVALC", "SEX")), selected = "AVALC",
    fixed = FALSE),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
  default_responses = c("CR", "PR", "Y", "Complete Response (CR)",
    "Partial Response (PR)", "M"),
  rsp_table = FALSE,
  control = list(global = list(method = ifelse(rsp_table, "clopper-pearson", "waldcc"),
    conf_level = 0.95), unstrat = list(method_ci = ifelse(rsp_table, "wald", "waldcc"),
    method_test = "schouten", odds = TRUE), strat = list(method_ci = "cmh", method_test =
    "cmh")),
  add_total = FALSE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  denom = c("N_col", "n", "N_row"),
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

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

- default_responses:

  (`list` or `character`)  
  defines the default codes for the response variable in the module per
  value of `paramcd`. A passed vector is transmitted for all `paramcd`
  values. A passed `list` must be named and contain arrays, each name
  corresponding to a single value of `paramcd`. Each array may contain
  default response values or named arrays `rsp` of default selected
  response values and `levels` of default level choices.

- rsp_table:

  (`logical`)  
  whether the initial set-up of the module should match `RSPT01`.
  Defaults to `FALSE`.

- control:

  (named `list`)  
  named list containing 3 named lists as follows:

  - `global`: a list of settings for overall analysis with 2 named
    elements `method` and `conf_level`.

  - `unstrat`: a list of settings for unstratified analysis with 3 named
    elements `method_ci` and `method_test`, and `odds`. See
    [`tern::estimate_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff.html),
    [`tern::test_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff_test.html),
    and
    [`tern::estimate_odds_ratio()`](https://insightsengineering.github.io/tern/latest-tag/reference/odds_ratio.html),
    respectively, for options and details on how these settings are
    implemented in the analysis.

  - `strat`: a list of settings for stratified analysis with elements
    `method_ci` and `method_test`. See
    [`tern::estimate_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff.html)
    and
    [`tern::test_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff_test.html),
    respectively, for options and details on how these settings are
    implemented in the analysis.

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

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `N_col`: total number of patients in this column across rows.

  - `n`: number of patients with any occurrences.

  - `N_row`: total number of patients in this row across columns.

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

- The display order of response categories inherits the factor level
  order of the source data. Use
  [`base::factor()`](https://rdrr.io/r/base/factor.html) and its
  `levels` argument to manipulate the source data in order to
  include/exclude or re-categorize response categories and arrange the
  display order. If response categories are `"Missing"`,
  `"Not Evaluable (NE)"`, or `"Missing or unevaluable"`, 95% confidence
  interval will not be calculated.

- Reference arms are automatically combined if multiple arms selected as
  reference group.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_binary_outcome(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiS1xH5RSUfUa7UA+h5e1kpB3oY6AO60pAAWtBDsYbg6IEo6OnRMLBz8qNSsjIoQGVnMbOzGRIwwnuSMosUZAIIAIgDKADI+OqQwBP5wAB7+UPyi1Ok6bVjtPX0Dw6P8DToApAB8a1MZMIJe5Ow7GdMAas2dAMI6ALw6-P4kxP4NqP7UUPRw1OzN51c2TbbEonUHROLvT7fI5gS6xFhQdSqHRYMTCUgAegAYgl+AkAOYKMBNE6ArbHYy0aj1dgABWaWGaAFlLq0dABCO5EgDypywAEkAHKnIk6AA+Z357X5tluXLAWO5nU63IA6joHLSicUAL7FABWRAS-gA1nBWKIkp4oDYIvw4MYoOj-AQ8aIBobjWaLcBoPBLWE5ABdNxtLo9MLAYBEsOdIlBkMQGZzO3WqMx1qzeOJpQsGAvB0uogwVA9OiiUhHEEMllsu7lyuMB23HQxrBMnQAISJKWIJZbBBhNemPdbYGHl21clwU2H9doFfYTeMLaJncQOlpHwIcHoRFHfdLd0HMY3rUYgnxOgAGqOiZcN5di-QEp5LBAp0pimhSxEEjEqwyMIW2SKY+CEERRBbcDhDEQCTj6fxSH8F9oDYR59j7OB4NBD4vmoVcwFRURUBIe0Gh7Y4gOtP1dHlZNKJBUFUARfp+AHWIjR3UR-FEb44CRfgcNBHQCE4mQxBbTRuEEOAXXE7jfkzdoUiJelGVrO8wHU5ltRnJiRL4kRBMIzsnHaWZaX5IkqJ0adbLzfxpMYDiuLEXj+ME4TQTEtyoLuZzaE+ER5L8pTZl7Id2y0mtWRiy5bFi1op30kSTiMgTyHY+jorAWz7IMjJHOXIt+zuYrC0PVLDNIZgvCclhXIkniMq82yMl85qpJYIL6BCzrFOTSKiXaJxbzwMdOyZLAAGksAAJi0xlLhcYkCrSjJWqywjltW-LqtBe1HWdJsSJIPj-MyBdK3artzMs-kWwbbyRNeAcYSfEsRHIFExFIiA+J0dhLiwRQJrUlgNG4X6zoB3Q6VBvTbtKOBtGoS6T0Kjb72LfI4B+4j-sB4HEfB7TIaCgjCfO+HaVJg6NoyEavD63RWgXOAoGJ9pWjB1TtMYIh8VO0QtDZjmudp3mbKx0ESTS9aNqFU4nFledF2RnQ3uPGF2hZkQdHZvjJaBnm+bHQUiFIHQnGk6hBGC+HBScMHFcZkQ0Yxl60pxr78d0am4aBkHzaJS3rdtmTHaB53Q-Jxgoapv6aaBum+c1pmBaFkWxcNiXidpaWyb16Ojc57mi81+WRLdkTeQFYUnuu72Tm10SYVpQXhbEUXtDz42C6L-mS9Z-vy-hs2kdlk4Pe+L21K7nO+7Lk26SHscR4NleK7j8Obbth3R-YWPtVu6uMlroCKGLQjBSLSY8oMklimKWgV3YBJ6kRDRtGsGw0hBKIeIEBWDNHQOwH8AASQQtAUhQL4owbQRQlA6iUGAHUQYgA)

## Examples

``` r

data <- teal_data()
data <- within(data, {
  library(dplyr)
  library(formatters)
  ADSL <- tmc_ex_adsl
  ADRS <- tmc_ex_adrs %>%
    mutate(
      AVALC = d_onco_rsp_label(AVALC) %>%
        with_label("Character Result/Finding")
    ) %>%
    filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]

arm_ref_comp <- list(
  ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
  ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
)
app <- init(
  data = data,
  modules = modules(
    tm_t_binary_outcome(
      label = "Responders",
      dataname = "ADRS",
      paramcd = choices_selected(
        choices = value_choices(ADRS, "PARAMCD", "PARAM"),
        selected = "BESRSPI"
      ),
      arm_var = choices_selected(
        choices = variable_choices(ADRS, c("ARM", "ARMCD", "ACTARMCD")),
        selected = "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      strata_var = choices_selected(
        choices = variable_choices(ADRS, c("SEX", "BMRKR2", "RACE")),
        selected = "RACE"
      ),
      default_responses = list(
        BESRSPI = list(
          rsp = c("Complete Response (CR)", "Partial Response (PR)"),
          levels = c(
            "Complete Response (CR)", "Partial Response (PR)",
            "Stable Disease (SD)", "Progressive Disease (PD)"
          )
        ),
        INVET = list(
          rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
          levels = c(
            "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
            "Progressive Disease (PD)", "Stable Disease (SD)"
          )
        ),
        OVRINV = list(
          rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
          levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
        )
      ),
      denom = "N_col"
    )
  )
)
#> Initializing tm_t_binary_outcome
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
