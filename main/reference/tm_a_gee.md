# teal Module: Generalized Estimating Equations (GEE) analysis

This module produces an analysis table using Generalized Estimating
Equations (GEE).

## Usage

``` r
tm_a_gee(
  label,
  dataname,
  parentname = "ADSL",
  aval_var,
  id_var,
  arm_var,
  visit_var,
  cov_var,
  arm_ref_comp = NULL,
  paramcd,
  conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
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

- aval_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the
  analysis variable.

- id_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object specifying the variable name for subject id.

- arm_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- visit_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `visit` variable. Must be a factor in
  `dataname`.

- cov_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  covariates variables.

- arm_ref_comp:

  (`list`) optional,\
  if specified it must be a named list with each element corresponding
  to an arm variable in `ADSL` and the element must be another list
  (possibly with delayed
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::variable_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)
  and
  [`teal.transform::value_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/value_choices.html)
  are deprecated but still accepted) with the elements named `ref` and
  `comp` that define the default reference and comparison arms when the
  arm variable is changed.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- conf_level:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  available confidence levels and default selection, each in the range
  (0, 1).

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

- `table` (`ElementaryTable` - output of
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_a_gee(
       ..., # arguments for module
       decorators = list(
         table = teal_transform_module(...) # applied to the `table` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiS1xH5RSUfUa7UA+h5e1kpB3oY6AO60pAAWtBDsYbg6IEo6OnRMLBx+vALCYhJSMvLpmQzMbEmo1KyMihAZAIIAIgDKADI+OqQwBP5wAB7+UPyi1OVtAIrtPX0Dw6P8AI6iOgCkAHwb5RnGtNTkjOzNAEKdAGLdAIQAvDoKYACaTzoAZDrnV50ATDr3R4vJ42ba7JoZHQwQRecjsPaQ5oANQAku0UbYdA8oOJjFB1EQTsi0Ri5LgES1UejbAA5LE6ZgQADWpypGJpoJ2FMhOIweIJJ054MhIp0vIggngjBk1k2XIhorFuPxpEJ1nJCsRSOanTOKLpD2ROv0OgArAAGHQAYh0AClBKJSEqxU1hrBarpVToAOZwJ3eegJHI6Cj8VBEBKkDAIoUI-iMIge7TUUQhCAAX0aACsIxB-Ey4KxU2EbBF+HA8cJSP4CPxaKIBjmEvnC6JgNB4MXPFA5ABdNzcAKabiCMQ9UzmDRWIY2My0Faj9gzpSSHGpwf+YfUUeiUtGNCoUPsVeidfUIcjsRkoHl0QGcvUKCsOD8EED9A9BIxeEQsL05LlHwQgiOsDxAYUqYIn0oz+L6cA-oqj70HA1D0k8ADiTguHg3JhB2ugPE8MztE8GqKlAW6biw9LDtKUD0CBx6xBGBBjoRYBGrq+ogmRoq0PwVGMDRLC0PRjEEMxMhsToBDwmADjtA4Zy2iirSkUCinKapPE6KIKFwOoL5ofJmkqWpYBktyLAwIJwl0QxYhMSx0myURWAALLqW57kAMLmdeekiIZ-DGc0Hk8dymj1jEtkPLRokOamEnOaBMlycS1JeRxbK0jpgUGeQIXsRlGIRZqGSoCwsC1vSqAyEykHlZC8ViY5yVSalrlgAACmFzS+eZKRPL1WD9SCllNRkW47uwG7TWIQ1gJcADS6IGJcADqZxlYqGQSQZTKBN2elOg8lw6u0LhNRNirEJosU6C1iUIbt7WsZ1clnM0l1Zc0mFZZdAAaWVfZdiAlbYO27flwX0jSDidJ03KNCKKM6I0jS0MYOjsJGqgqlo8FyDYaQQqI8QQKwzToLN6AACSCLQKQHnTemMNoDRKOmShgOmvZAA)

## Examples

``` r

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADQS <- tmc_ex_adqs %>%
    filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
    mutate(
      AVISIT = as.factor(AVISIT),
      AVISITN = rank(AVISITN) %>%
        as.factor() %>%
        as.numeric() %>%
        as.factor(),
      AVALBIN = AVAL < 50 # Just as an example to get a binary endpoint.
    ) %>%
    droplevels()
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

all_values <- function(x) unique(x)
class(all_values) <- append(class(all_values), "des-delayed")

app <- init(
  data = data,
  modules = modules(
    tm_a_gee(
      label = "GEE",
      dataname = "ADQS",
      aval_var = variables(choices = "AVALBIN"),
      id_var = variables(choices = c("USUBJID", "SUBJID"), selected = "USUBJID"),
      arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
      visit_var = variables(choices = c("AVISIT", "AVISITN"), selected = "AVISIT"),
      paramcd = picks(
        variables(choices = c("PARAMCD", "PARAM")),
        values(all_values, "FKSI-FWB"),
        check_dataset = FALSE
      ),
      cov_var = variables(
        choices = c("BASE", "AGE", "SEX", "BASE:AVISIT"),
        selected = NULL
      )
    )
  )
)
#> Initializing tm_a_gee (prototype)
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
