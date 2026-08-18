# teal Module: Multiple Events by Term

This module produces a table of multiple events by term.

## Usage

``` r
tm_t_mult_events(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  seq_var,
  hlt,
  llt,
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  event_type = "event",
  title_text = "Concomitant Medications",
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

  (`character`)\
  menu item label of the module in the teal app.

- dataname:

  (`character`)\
  analysis data used in teal module.

- parentname:

  (`character`)\
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- seq_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as analysis sequence number variable. Used for
  counting the unique number of events.

- hlt:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  name of the variable with high level term for events.

- llt:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  name of the variable with low level term for events.

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- na_level:

  (`string`)\
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- event_type:

  (`character`)\
  type of event that is summarized (e.g. adverse event, treatment).
  Default is `"event"`.

- title_text:

  (`string`)\
  text to display as the first part of the dynamic table title. The
  table title is constructed as follows: "`title_text` by `hlt` and
  `llt`". Defaults to `"Concomitant Medications"`.

- drop_arm_levels:

  (`logical`)\
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- pre_output:

  (`shiny.tag`) optional,\
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional,\
  with text placed after the output to put the output into context. For
  example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- basic_table_args:

  (`basic_table_args`) optional\
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
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_mult_events(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOyAYQBZIpKyyur+AhglAF9FCAArIni-AGs4VlFE0Jtw-jhjKGFSPwJ+WlEy0fGpmeBoeFmkuQBdJRreyenRIoJ2BTBc2wdsgE0ASWznlOeHLkHAAhABSPz+OmemVe2VsLUhz1auScAEVEWBMrYmgBGDFYpoAJnx2IAzCSmgAWZ5DPYQW4zOaeOTAaHNBF4KGY9nPc5Fa4wBmiVwQHIFIpJYCs7kFXmXUXsiWhKVs1pykVoVBFeLRJ4QDJJHQAXh0SVwDT4QhE9xNluEYj1GQyJT86xga3K2jIswaTp01Cg9Dg1GNXKaJGIMGiUDIOhacC2sg0JHu9FYcYTMg8ln1TQDonuMf4OgACoxlqpy8WAHKwOB-X1OpLHXQm1Uc836v06FiCzQsUP9xi0QPW9gEGJjAhiUOPaFYDtczILpq-MByFKiYNwdQJ0PzjkbxsZLcARz8Q8HLBH9DHE6nM7bYGRaMhW5Eu+LT5f6M5xloFR7ia9jOEeXZ+jE1CkFew6jg6x5OveMiPjoc6YtieKctC2LElh6FNOSeEEtS66dt2Trvju5BfqhTz4Zh-z4bhjEEoRLHYiRQzkWB5HUFBME3mOCEZEh042mGLTZE4TQAPJrmR5E6JRn77s+knSXJzwITx3Y1PwrpEJ4IbAY4TgKX6cBeuspCsKgrZcqQ5YePAZBaeBOhcR5ShDLQxg6Ow8TkGoGjaNYNhpF2ohxBArCZOg7CagAJIItApElW6MNojBDP0ShgP05xAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADCM <- tmc_ex_adcm
})
join_keys(data) <- default_cdisc_join_keys[names(data)]
adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
join_keys(data)["ADCM", "ADCM"] <- adcm_keys

ADSL <- data[["ADSL"]]
ADCM <- data[["ADCM"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_mult_events(
      label = "Concomitant Medications by Medication Class and Preferred Name",
      dataname = "ADCM",
      arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
      seq_var = variables(choices = "CMSEQ", selected = "CMSEQ", fixed = TRUE),
      hlt = variables(
        choices = c("ATC1", "ATC2", "ATC3", "ATC4"),
        selected = c("ATC1", "ATC2", "ATC3", "ATC4")
      ),
      llt = variables(
        choices = "CMDECOD",
        selected = "CMDECOD"
      ),
      add_total = TRUE,
      event_type = "treatment"
    )
  )
)
#> Initializing tm_t_mult_events
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
