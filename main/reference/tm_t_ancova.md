# teal Module: ANCOVA Summary

This module produces a table to summarize analysis of variance,
consistent with the TLG Catalog template for `AOVT01` available
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/aovt01.html)
when multiple endpoints are selected.

## Usage

``` r
tm_t_ancova(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  arm_ref_comp = NULL,
  aval_var,
  cov_var,
  include_interact = FALSE,
  interact_var = NULL,
  interact_y = FALSE,
  avisit,
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

- arm_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

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

- aval_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the
  analysis variable.

- cov_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  covariates variables.

- include_interact:

  (`logical`)\
  whether an interaction term should be included in the model.

- interact_var:

  (`character`)\
  name of the variable that should have interactions with arm. If the
  interaction is not needed, the default option is `NULL`.

- interact_y:

  (`character`)\
  a selected item from the interact_var column which will be used to
  select the specific `ANCOVA` results when interact_var is discrete. If
  the interaction is not needed, the default option is `FALSE`.

- avisit:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  value of analysis visit `AVISIT` of interest.

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

## Details

When a single endpoint is selected, both unadjusted and adjusted
comparison are provided. This modules expects that the analysis data has
the following variables:

- `AVISIT`: variable used to filter for analysis visits.

- `PARAMCD`: variable used to filter for endpoints, after filtering for
  `paramcd` and `avisit`, one observation per patient is expected for
  the analysis to be meaningful.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_ancova(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOyARVyikrLK6v4AR1ElAF9FCAArIni-AGs4VlFE0Jtw-jhjKGFSPwJ+WlEy0fGpmeBoeFmkuQBdVwgcgqKk4GAFMBv8p-PL6+bWxdCHp5yWm8PkoWDA-IxlhsiDBUEU6KJSOwGpksABZHQAXh08MRDQyEOMmJ0TwAQogdAAFahQAhwehEJ64PE6YgwokEJHPcnZRiCADmOgAGoziWAAMLksXQ+jxDyWCBPIYZORMiAZTJi2wo1Fi7JEnFItUZHQEon-NE6EmM5ms2FYjnm9GZEWOnRixUNIZDEHoIrxaKGjJJIlJVUZPhCESiIkR4RiQPGkp+dZQcxETRQBPG7FQehwahm54AOTFAHkAGqZOy5kTWo3ZpLHXRY-5fOvZjJQTTbaJE1AyCazZnZjOMWg1+P-csASVy09sLueM7nC7AKuHxoz1EE8dE+bg6jg-ELAHUnE4ANI6ACMOmymQAmjoABwimBrWioERE+zOdf1jsCBiA8JgCUI91IIkADFMnyXIXAA5Uww7UE-FHIlR3Hego3YB1njRRcNS1NFdUIkjskVFI9xEQ9jxbfCdQotdkOzVCCShNksTYyFbRY40u24NCWAwlgsJwvCxQACQAcUIytXmYnRqIPcg6NFKTZLwHR32oDQv2bHQYLgpx-w7Fl0yExgRLHCdZjwklMngxdcnsTItWvZynGFRTlNowsXKwNzMg85iN1QFhYE2PsByHRDN1E2zOQpFFMkYxdksCtLQrijItx3WZfNUwsoIvOcDCgk8rS0nS9O-LFfxMvjsyAkCwM8CDoNgpy4tMjt4nINR1nQrFMMS+zHJcLSngCoKQpSaavMopT9z8+iZvct8P30zrjKVY09q9JRaEJdh+tUGkNG0awbDSI1RDiCBWEydB2DQVAABJBFoFI3vevdGG0Rghn6JQwH6c4gA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADQS <- tmc_ex_adqs
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADQS <- data[["ADQS"]]

arm_ref_comp <- list(
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  ),
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_t_ancova(
      label = "ANCOVA Table",
      dataname = "ADQS",
      avisit = picks(
        variables("AVISIT", "AVISIT"),
        values(selected = "WEEK 1 DAY 8", multiple = TRUE),
        check_dataset = FALSE
      ),
      arm_var = variables(c("ARM", "ACTARMCD", "ARMCD"), selected = "ARMCD"),
      arm_ref_comp = arm_ref_comp,
      aval_var = variables(c("CHG", "AVAL"), selected = "CHG", multiple = FALSE),
      cov_var = variables(c("BASE", "STRATA1", "SEX"), selected = "STRATA1"),
      paramcd = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "FKSI-FWB", multiple = TRUE),
        check_dataset = FALSE
      ),
      interact_var = variables(c("BASE", "STRATA1", "SEX"), selected = "STRATA1", multiple = FALSE)
    )
  )
)
#> Initializing tm_t_ancova
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
