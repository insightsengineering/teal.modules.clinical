# teal Module: Patient Profile Laboratory Table

This module produces a patient profile laboratory table using ADaM
datasets.

## Usage

``` r
tm_t_pp_laboratory(
  label,
  dataname = "ADLB",
  parentname = "ADSL",
  patient_col = "USUBJID",
  timepoints = NULL,
  aval = lifecycle::deprecated(),
  aval_var = NULL,
  avalu = lifecycle::deprecated(),
  avalu_var = NULL,
  param = NULL,
  paramcd = NULL,
  anrind = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = lifecycle::deprecated()
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

- patient_col:

  (`character`)  
  name of patient ID variable.

- timepoints:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the time
  variable from `dataname`.

- aval:

  **\[deprecated\]** Please use the `aval_var` argument instead.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- avalu:

  **\[deprecated\]** Please use the `avalu_var` argument instead.

- avalu_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  analysis unit variable.

- param:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `PARAM` variable from `dataname`.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- anrind:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `ANRIND` variable from `dataname`. Variable should have the following
  3 levels: `"HIGH"`, `"LOW"`, and `"NORMAL"`.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XCByCoqTgYAUwa-zHs4urptaF0PvHnJbXu8lGhUEV4tF2A0kjoALw6JK4Bp8IQiUSwnTI4RiSEQDIZEp+NboPzUKD0IjMUgUjgNPE6Un0ODUdGPABq0W4okeiNxdPhoSOujhf0+3NpeNQHloFDWxGZwrADlyDmaACkAJLZMW8umS5ilfjoggxUYEMR+URMuDqOD8HF8vHG01idGaFi0MkidYmmTY-7NFKPAAKmSwmQAsgBhLVgOQ8h0ZS0iG2GhUhsNRmPijJx7M6PWwI0+s2iC1WlP2hNO31ouFuxge+he6sl9j+wNgdMRx65nV8pPW8ipnTB0PdsB53sOjTwVCjMi1nQt80Dit5jLLxf1xvN4t+z4dnIATR78YTq6HLKe2RPE77ObPdKgEAbEGHm7LyaHlYdm9d7s9OBvWdGZ2xHJ4ADksHVCCYync9y0vBVMigmCs3vHR4KfN1-HrIsQM-QdbR-Pk-zrACmyAzc2wPcDMlZTIXljR9+0Q20r3oxjHknFiMigHDBD8PC4Q-C9iPXJc9y3CjdxAmiWkPBj8gcU8JLE4c-iUlS7wdQY6T0zClEGWhjB0dh4nINQNG0awbDSXlRDiCBWEydB2BBAASQRaBSTzLUYbRGEGPolDAPoziAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_pp_laboratory(
      label = "Vitals",
      dataname = "ADLB",
      patient_col = "USUBJID",
      paramcd = choices_selected(
        choices = variable_choices(ADLB, "PARAMCD"),
        selected = "PARAMCD"
      ),
      param = choices_selected(
        choices = variable_choices(ADLB, "PARAM"),
        selected = "PARAM"
      ),
      timepoints = choices_selected(
        choices = variable_choices(ADLB, "ADY"),
        selected = "ADY"
      ),
      anrind = choices_selected(
        choices = variable_choices(ADLB, "ANRIND"),
        selected = "ANRIND"
      ),
      aval_var = choices_selected(
        choices = variable_choices(ADLB, "AVAL"),
        selected = "AVAL"
      ),
      avalu_var = choices_selected(
        choices = variable_choices(ADLB, "AVALU"),
        selected = "AVALU"
      )
    )
  )
)
#> Initializing tm_t_pp_laboratory
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
