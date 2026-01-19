# teal Module: Adverse Events Table by Standardized MedDRA Query

This module produces an adverse events table by Standardized MedDRA
Query.

## Usage

``` r
tm_t_smq(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  id_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "USUBJID"), selected = "USUBJID", fixed = TRUE),
  llt,
  add_total = TRUE,
  total_label = default_total_label(),
  sort_criteria = c("freq_desc", "alpha"),
  drop_arm_levels = TRUE,
  na_level = tern::default_na_str(),
  smq_varlabel = "Standardized MedDRA Query",
  baskets,
  scopes,
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
  names that can be used as `arm_var`. It defines the grouping
  variable(s) in the results table. If there are two elements selected
  for `arm_var`, second variable will be nested under the first
  variable.

- id_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object specifying the variable name for subject id.

- llt:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  name of the variable with low level term for events.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- sort_criteria:

  (`character`)  
  how to sort the final table. Default option `freq_desc` sorts on
  column `sort_freq_col` by decreasing number of patients with event.
  Alternative option `alpha` sorts events alphabetically.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- smq_varlabel:

  (`character`)  
  label to use for new column `SMQ` created by
  [`tern::h_stack_by_baskets()`](https://insightsengineering.github.io/tern/latest-tag/reference/h_stack_by_baskets.html).

- baskets:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected options for
  standardized/customized queries.

- scopes:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices for the scopes of standardized
  queries.

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

    tm_t_smq(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHTomFg5fXgFhMQkpGXl0zIZmNkTUalZGRQgMrKqORk96EVFGjIBBABEAZQAZbx1SGAI-OAAPPyh+UWpygd6nMYmp2fn3OFcmnQxoeFE-eihRAGs4UlExgHNGOFR2BTAAPXZBgFkARQAfADCvzkGAAVAA5XrfAAkbxSxzE7FWTjkKU03EEugAvHZHKjykdYGI-KJiKgxA8ni83u8fr9wYNAXC8DpEaJkf01midBjqFidLj7M5GoSCKdzlcbndwgQYkQZCTRHAROo4PxXgcMnKFQRKbiMYxaFBOnA-DrFRyUSlRIJ6MrSILDuyzhdrrc0eUMsrVeR+E6iSdXVLbuVRQcJKcyUQKTKjBa9VGVXA1RqvToE-reSxjabzfLLZy1ja7Q6Ay7o7HPVqdD6U37y8So+SxLh08ZaDN1U7hS4Do0AL6NABWCogfmurA5SRs4X4cGMUGEpHN-FoZL8o-iE7gU+A7MSoTkAF19mhUGN4tFNRkkk6km2DnwhF0nc-Chz0xM-CvRDAAI43hkwHUCaKpOm8vT8NojDKjoTjaGQdz0KwOj0nYJoiPC6a3qEiIQWAKLYTWGQsDAfiGk6mZJr66pAcBwHUU6hq5iI+a6kiSTAMAkFDMMbzHseJb2jcVGvIRWDfPCOhvIMTgABpvHI1YMQxdapgRvSSW8OE6CpqkLPwP5EJ41BOgAYr0wxyY+qk6JK7p3LiXE8WAkbBo5AlCbplZZi5bzub5ohebZqnUNQjq4tRpLJqm9GqUxBo5phZrUYenjcbxawhbWpaiVF4lrP0TiAgA8v0Sn6XZ6kNrikFOMVZUVWAuk9MBbWNI0tDGDo7DxOQagaNo1g2GkByiHEECsL06DsOeMKCLQKTzcqjAwYOShgAOx5AA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  library(rtables)
  ADSL <- tmc_ex_adsl
  ADAE <- tmc_ex_adae

  .names_baskets <- grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
  .names_scopes <- grep("^SMQ.*SC$", names(ADAE), value = TRUE)

  .cs_baskets <- choices_selected(
    choices = variable_choices(ADAE, subset = .names_baskets),
    selected = .names_baskets
  )

  .cs_scopes <- choices_selected(
    choices = variable_choices(ADAE, subset = .names_scopes),
    selected = .names_scopes,
    fixed = TRUE
  )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_smq(
      label = "Adverse Events by SMQ Table",
      dataname = "ADAE",
      arm_var = choices_selected(
        choices = variable_choices(data[["ADSL"]], subset = c("ARM", "SEX")),
        selected = "ARM"
      ),
      add_total = FALSE,
      baskets = data[[".cs_baskets"]],
      scopes = data[[".cs_scopes"]],
      llt = choices_selected(
        choices = variable_choices(data[["ADAE"]], subset = c("AEDECOD")),
        selected = "AEDECOD"
      )
    )
  )
)
#> Initializing tm_t_smq
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
