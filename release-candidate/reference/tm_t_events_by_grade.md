# teal Module: Events by Grade

This module produces a table to summarize events by grade.

## Usage

``` r
tm_t_events_by_grade(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  hlt,
  llt,
  grade,
  grading_groups = list(`Any Grade (%)` = c("1", "2", "3", "4", "5"), `Grade 1-2 (%)` =
    c("1", "2"), `Grade 3-4 (%)` = c("3", "4"), `Grade 5 (%)` = "5"),
  col_by_grade = FALSE,
  prune_freq = 0,
  prune_diff = 0,
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  drop_arm_levels = TRUE,
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

- hlt:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  name of the variable with high level term for events.

- llt:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  name of the variable with low level term for events.

- grade:

  (`character`)  
  name of the severity level variable.

- grading_groups:

  (`list`)  
  named list of grading groups used when `col_by_grade = TRUE`.

- col_by_grade:

  (`logical`)  
  whether to display the grading groups in nested columns.

- prune_freq:

  (`number`)  
  threshold to use for trimming table using event incidence rate in any
  column.

- prune_diff:

  (`number`)  
  threshold to use for trimming table using as criteria difference in
  rates between any two columns.

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

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

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

- `table` (`TableTree` as created from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_events_by_grade(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHTomFg5fXgFhMQkpGXl0zIZmNk44bgwkxQgMrKqORk96EVFG5sqcxNRqVkYenQBBABEAZQAZbx1SGAI-OAAPPyh+UWpyjGpO0Q33XXDif2ooejhqUU4llfXNqDhRybGnecXltaPnnQBSAB8-3KGRggk85D8tGM7Fo4gIMRYUHUqhSUHExhRpCIIx0AGIAOQ6K46AiqTzxHREYw6JGaeIAcxM2NxonKZz8FyuN3YbycNnCewOvzgSgAvo0AFZEeJ+ADWcFYtwa834cCxwlIfgI-HhyxlcsVyuA0HgKtCcgAuq4IJNZmrQsBgAowPaZq6rTa7RN3o7PM7XfzPd6lGhUPN4tF2OUkjoALw6JK4cp8IRdBM6NOFW6ghYwPzauDaMiHeisPyM5jqmNNDL17nXTNB-jaRiiXROEukUQk1g6ADi1d0tkuIldKbr9aToTNukTQd9LjweYyLALmhYmcRsvJhw7IlR-HYBBjbqwAFkJzog5eAMITV1yFK3q9gZ+rzLUUjbmK7sR+AecBHrW07TjuMhiJmm6MLQY5wDqf6Qbc-IpKeQZOLYTiXteGETE4d4APKPu+H5TmBQFHtuZ7vPhREkaM05kWBdLfr+-77tcwHkMen4ZBBe7QSwcGdAhAliHyS5oTRTgAELEVMACaUy4W6ThTIRd5PsxLE6JRPHNmp8nTMprqfjp05Vps85kkhe6AVxIF8bZHFCbB8GIRxknvNJGG2IRAAaA5YKp7xTE4ABq2mTrpemOQZC5qf5QUhWA5l5qMjSNDCOhwioagaNo1g2GkdaiHEECsGM6DsOGAAkgi0Oi6B1R2jBto04pKGA4pWkAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(teal.data)
  library(rtables)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  .lbls_adae <- col_labels(tmc_ex_adae)
  ADAE <- tmc_ex_adae %>%
    mutate_if(is.character, as.factor) #' be certain of having factors
  col_labels(ADAE) <- .lbls_adae
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_events_by_grade(
      label = "Adverse Events by Grade Table",
      dataname = "ADAE",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      llt = choices_selected(
        choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
        selected = c("AEDECOD")
      ),
      hlt = choices_selected(
        choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
        selected = "AEBODSYS"
      ),
      grade = choices_selected(
        choices = variable_choices(ADAE, c("AETOXGR", "AESEV")),
        selected = "AETOXGR"
      )
    )
  )
)
#> Initializing tm_t_events_by_grade
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
