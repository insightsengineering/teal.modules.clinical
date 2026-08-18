# teal Module: Abnormality Summary Table

This module produces a table to summarize abnormality.

## Usage

``` r
tm_t_abnormality(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  by_vars,
  grade,
  abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
  id_var = teal.picks::variables("USUBJID", "USUBJID", fixed = TRUE),
  baseline_var = teal.picks::variables("BNRIND", "BNRIND", fixed = TRUE),
  treatment_flag_var = teal.picks::variables("ONTRTFL", "ONTRTFL", fixed = TRUE),
  treatment_flag = teal.picks::values("Y", "Y", fixed = TRUE, multiple = FALSE),
  add_total = TRUE,
  total_label = default_total_label(),
  exclude_base_abn = FALSE,
  drop_arm_levels = TRUE,
  pre_output = NULL,
  post_output = NULL,
  na_level = tern::default_na_str(),
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

- by_vars:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option(s) for row-by
  variables (`multiple = TRUE`, `ordered = TRUE` recommended).

- grade:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  abnormality grade variable. Variable must be factor.

- abnormal:

  (`named list`)\
  defined by user to indicate what abnormalities are to be displayed.

- id_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object specifying the variable name for subject id.

- baseline_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable for baseline abnormality grade.

- treatment_flag_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  on treatment flag variable.

- treatment_flag:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  value(s) indicating on-treatment records in `treatment_flag_var` (e.g.
  `"Y"`).

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- exclude_base_abn:

  (`logical`)\
  whether to exclude patients who had abnormal values at baseline.

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

- na_level:

  (`character`)\
  the NA level in the input dataset, defaults to `"<Missing>"`.

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

## Note

Patients with the same abnormality at baseline as on the treatment visit
can be excluded in accordance with GDSR specifications by using
`exclude_base_abn`.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_abnormality(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiS1xH5RSUfUa7UA+h5e1kpB3oY6AO60pAAWtBDsYbg6IEo6OnRMLBx+vALCYhJSMvLpmQzMbOzGRIwwnuSMoooQGVlVHPyo1KyMrRkAggAiAMoAMj46pDAE-nAAHv5Q-KLU5SPjAEJTM3OLy-zU9DoApAB8p+UZMIJe5OzXGToA8gBy9rYAYpMAvDqyURwfyRWIUR5tZ5QwYANQAkqM4bYzglTgDHmBRgBhLBOJxvOFvADiCjwOlJW0Goyc40JLjANgAfuSwKTcE8ofZnDpmaSAJqkjk6GwXNHROL+ahQehwagYl5tWyMOCeeBkHRYODERj8HRfKUAc1JA2erQAvq0AFZEBL+ADWcFYoiSnigNgi-DgxigwlI-gI-Foojm1ttDqdwGg8GdYTkAF03CMJlMwsBgKSk+NSXGExBNjsPa60xnhtts7mlGhUFMEjEIRkwjp-slynwhCJRE2dG3Cs6njN-H7pRA6g06KQOEKpTLqF2M-QR-VuDFWHZpSI2UKwlHdP8S2W8EKWDB-JoWF2z4xaOuxOwCLEbQQxF2CBjBlgALJslnvj9Y4bGikQIiOocC6nuYC-oBR78Pwg5EF4s7-F8gzjNS7KQlC9CsKeLCdv8l7XvQHb1lCzz3o+z7-K+pLbFigy2N+pIAArvoMX5khm8KIoxDIYWRzzAVq5DgeitFbPRvEpCxbEcXI-ECd2vq0D0u52I4TgKQJdSesqolci4mGmlpzz0FAwEJMCl4XiwREkRRMhUSyWxvFghIAWSxi0AsYFdgZ8lCgazCejZV43s6DlPvhP6ue535eT5+kaQFRkZMOo7cF2dCiKQ7DUEQkRzmA4wvAA6t+8QGrERUABJwkSNXQalOiLJIgiev4ZlAssC5dihaGGVCJqtK0tDGDo7AJE0UAWNo1g2GkkKiPEECsIM6DsFWAAkgi0Ck21Aow2j9EoZpKGAZpxkAA)

## Examples

``` r

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(formatters)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb %>%
    mutate(
      ONTRTFL = case_when(
        AVISIT %in% c("SCREENING", "BASELINE") ~ "",
        TRUE ~ "Y"
      ) %>% with_label("On Treatment Record Flag")
    )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_abnormality(
      label = "Abnormality Table",
      dataname = "ADLB",
      arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
      add_total = FALSE,
      by_vars = variables(
        choices = c("LBCAT", "PARAM", "AVISIT"),
        selected = c("LBCAT", "PARAM"),
        multiple = TRUE,
        ordered = TRUE
      ),
      baseline_var = variables(choices = "BNRIND", fixed = TRUE),
      grade = variables(choices = "ANRIND", fixed = TRUE),
      abnormal = list(low = "LOW", high = "HIGH"),
      exclude_base_abn = FALSE
    )
  )
)
#> Initializing tm_t_abnormality
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
