# teal Module: Mixed Model Repeated Measurements (MMRM) Analysis

This module produces analysis tables and plots for Mixed Model Repeated
Measurements.

## Usage

``` r
tm_a_mmrm(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  aval_var,
  id_var,
  arm_var,
  visit_var,
  cov_var,
  arm_ref_comp = NULL,
  paramcd,
  method = teal.transform::choices_selected(c("Satterthwaite", "Kenward-Roger",
    "Kenward-Roger-Linear"), "Satterthwaite", keep_order = TRUE),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
  plot_height = c(700L, 200L, 2000L),
  plot_width = NULL,
  total_label = default_total_label(),
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args(),
  ggplot2_args = teal.widgets::ggplot2_args(),
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

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- id_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object specifying the variable name for subject id.

- arm_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- visit_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as `visit` variable. Must be a factor in
  `dataname`.

- cov_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  covariates variables.

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

- method:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  adjustment method.

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

- plot_height:

  (`numeric`) optional  
  vector of length three with `c(value, min, max)`. Specifies the height
  of the main plot and renders a slider on the plot to interactively
  adjust the plot height.

- plot_width:

  (`numeric`) optional  
  vector of length three with `c(value, min, max)`. Specifies the width
  of the main plot and renders a slider on the plot to interactively
  adjust the plot width.

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

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for all the plots or named list of `ggplot2_args`
  objects for plot-specific settings. List names should match the
  following: `c("default", "lsmeans", "diagnostic")`. The argument is
  merged with option `teal.ggplot2_args` and with default module
  arguments (hard coded in the module body). For more details, see the
  help vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

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

## Note

The ordering of the input data sets can lead to slightly different
numerical results or different convergence behavior. This is a known
observation with the used package `lme4`. However, once convergence is
achieved, the results are reliable up to numerical precision.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `lsmeans_plot` (`ggplot`)

- `diagnostic_plot` (`ggplot`)

- `lsmeans_table` (`TableTree`- output from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

- `covariance_table` (`ElementaryTable`- output from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

- `fixed_effects_table` (`ElementaryTable`- output from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

- `diagnostic_table` (`ElementaryTable`- output from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_a_mrmm(
       ..., # arguments for module
       decorators = list(
         lsmeans_plot = teal_transform_module(...), # applied only to `lsmeans_plot` output
         diagnostic_plot = teal_transform_module(...), # applied only to `diagnostic_plot` output
         lsmeans_table = teal_transform_module(...), # applied only to `lsmeans_table` output
         covariance_table = teal_transform_module(...), # applied only to `covariance_table` output
         fixed_effects_table = teal_transform_module(...), # applied only to `fixed_effects_table` output
         diagnostic_table = teal_transform_module(...) # applied only to `diagnostic_table` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiS19EYwA+ozjHPxGFR9IzpRUnYlHR0AQSwAWQBhABEdAF4dUPDIqJ0fYzSdBTBYuJ0AISLcbKiAoPSCCOL4mMrCptKEosUIKO7upX4oUihgnS5qT0Hh6wGhkcMdAHdaUgALWgh2KahcHRBsuiYWDnHeAWExCSkZeQOGZjYt1GpWRm6o6KSAZQAZUdIYARPHAAB6eKD8UTUbKfACKX3+gOBYIhAEdRDoAKQAPkx1RMtGo5EY7GiZR+ADE-gBCdJFACaRR0ADIYuSqQAmHS0tqMsA2HF4no5YyE4mkgBqAEkvlLbFiNpidA0igB1JxOADSOgAjDoktF6ToABytNUa7Vcg1GnUAVjNYHVWp0AGZ9YadByOV0Bbj8TBBMNyBFhTkYtLZfL0lBxMYoOoiCTohG5XIqqGcsmZXKAHIFZgQADWkuzthzvqFYbDMYwcYTJIr+Or4gggngjBk1ixfozzdr8dIia7AGIdDAoIWNgBzZUkURwAiBrS6VvtmQmAeJ-HdAC+3QAVkQNp5C3BWKItnMbAt+L4oMJSP5+LRREDD8fT+fgNB4BftnIAF03DQIIFg2FYQyibYCm2dMoj4IQRAxdIEIuC98QBcFPBgGAPEgqtqCgeg4GoAoijiOJ4kqJsdG2H9dDpYoknhajex0KBNG4TxOMYAoCFWI8CDETx5xEdQ4H4dgVWKCVoh+B0EgACQAcS6XYimTOS1Jo2h+G4lg+IEmRhNEhdyEk6SHC+BwygAKSlJIHWsuyHLUtorJs+zHP5OCqxYLweMMwSTJIsyJKkxoSgdEpkjcjSqJ8mjNBfFZ9N4+ojKE0QRNC8SLMilNbGiwqczimTS20tj-O8Xx-CIQICmqvI6sCXyw1QFhYAIfgguM7LTLy-Cq2VTKxAKTjqEEOB-FGv85mAYANOYr4igAgD1LAAAFWJokSbyNu2rBdsq4aogG8yyLAClNVlAwKVVCowBotMaOITQ0t6rKcrE8yIqKMpoi+Fw8DaaJlOBjagYADQdAGgcQLNIzcnMHB+H53hyDG+iUWh8nYDZiQHZdrBsfZhVEdYIFYaJ0HYECABJBFoXYGfnRhtDeJQdyUMAdwAoA)

## Examples

``` r
arm_ref_comp <- list(
  ARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  )
)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADQS <- tmc_ex_adqs %>%
    filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
    filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
    mutate(
      AVISIT = as.factor(AVISIT),
      AVISITN = rank(AVISITN) %>%
        as.factor() %>%
        as.numeric() %>%
        as.factor() # making consecutive numeric factor
    )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_a_mmrm(
      label = "MMRM",
      dataname = "ADQS",
      aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
      id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        choices = value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
        selected = "FKSI-FWB"
      ),
      cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL)
    )
  )
)
#> Initializing tm_a_mmrm
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
