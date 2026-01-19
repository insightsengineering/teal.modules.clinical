# teal Module: Adverse Events Summary

This module produces an adverse events summary table.

## Usage

``` r
tm_t_events_summary(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  flag_var_anl = NULL,
  flag_var_aesi = NULL,
  dthfl_var =
    teal.transform::choices_selected(teal.transform::variable_choices(parentname,
    "DTHFL"), "DTHFL", fixed = TRUE),
  dcsreas_var =
    teal.transform::choices_selected(teal.transform::variable_choices(parentname,
    "DCSREAS"), "DCSREAS", fixed = TRUE),
  llt = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AEDECOD"), "AEDECOD", fixed = TRUE),
  aeseq_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AESEQ"), "AESEQ", fixed = TRUE),
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  count_dth = TRUE,
  count_wd = TRUE,
  count_subj = TRUE,
  count_pt = TRUE,
  count_events = TRUE,
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

- flag_var_anl:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  or `NULL`)  
  vector with names of flag variables from `dataset` used to count
  adverse event sub-groups (e.g. Serious events, Related events, etc.).
  Variable labels are used as table row names if they exist.

- flag_var_aesi:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  or `NULL`)  
  vector with names of flag variables from `dataset` used to count
  adverse event special interest groups. All flag variables must be of
  type `logical`. Variable labels are used as table row names if they
  exist.

- dthfl_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as death flag variable. Records with \`"Y"“ are
  summarized in the table row for "Total number of deaths".

- dcsreas_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as study discontinuation reason variable.
  Records with `"ADVERSE EVENTS"` are summarized in the table row for
  "Total number of patients withdrawn from study due to an AE".

- llt:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  name of the variable with low level term for events.

- aeseq_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  variable for adverse events sequence number from `dataset`. Used for
  counting total number of events.

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

- count_dth:

  (`logical`)  
  whether to show count of total deaths (based on `dthfl_var`). Defaults
  to `TRUE`.

- count_wd:

  (`logical`)  
  whether to show count of patients withdrawn from study due to an
  adverse event (based on `dcsreas_var`). Defaults to `TRUE`.

- count_subj:

  (`logical`)  
  whether to show count of unique subjects (based on `USUBJID`). Only
  applies if event flag variables are provided.

- count_pt:

  (`logical`)  
  whether to show count of unique preferred terms (based on `llt`). Only
  applies if event flag variables are provided.

- count_events:

  (`logical`)  
  whether to show count of events (based on `aeseq_var`). Only applies
  if event flag variables are provided.

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

    tm_t_events_summary(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHTomFg5fXgFhMQkpGXl0zIZmNkTUalZGRQgMrKrc1QhG5sqc9mMiRhgPckZRTp0AQQARAGUAGW8dUhgCPzgADz8oflFqHQBSAD498oyYQU9ydhOMnUnbAAkAMXmAXh1ZUTg-CJiKK6aboCAIS0cTQdh3e53GwAPx0CjAAE0EbhroD7M4dHCETiAYCbIc9pFojE-NQoPQ4NQrmBpoJ6AArODqW5wDwxHSPckAcwRYwyYym4ycCyWK3Wm3ccFcAIwW34q20ZD8xh5ogWpnMGisgRsaTxOkCCyNhLROjOFzg-0B6IAsgAFZ5+aZOLA6N7Cl1ul5vBHIvBmjK2B1OrBOV4TV3h92+pEowN2EOzPwAcSwAFZ3ZHbAB5AAaaZj8LA6fjBrt9udtoAigAGACMWZBYKg7GmNYbADlxra5Kjyzdg5X23WAExN0EYcEj2uj7u9-s2wcOvwAYTrjbezanrfXXZ7-Juh+I1EEMAgZIpVPV4ToolI1ptQ6dXqzCOmqksgnVwrLS+fyZhhGCJYFSHhwPwkZ-k+Saphmb5gCmzD8Loma-gGA5DlWG5ZlAYi0JelLUoEwDAO+Hb1vOCIALrUSkJFkbSFHTKuNHUX2ZpYTO45vHhogEeSRE0oiGAAJxiRJokAPQAFqSZJkbTAAknyi4wZWe6bjofECVexEeKRCKaVRYC0Yex5EP4gnXokHhyMA0DwKI7AnmeF7WdQozUQsUDiAQMQsFA6iqC5lluYR16HoE5QAL4yhkADEADkOgphQqgePE3I6N+qgGChxjxBBOhwEqpAmGqGDlEKIrhDV+xHDocr8AqpUUKQKpqsEEDlHKXxQBA-iaCwN5GI5YjsDV9n3iwpCiAA6iS7Djc5U0pAiAF+Hy1G9TpfjDSMCwrZNkzCtNniMHNi2xMtsATWtxZYdtSgxY0DJEPEfgANZwKwzlJDY4QFVAwgdQQ-Cgis72fT9f0OXd-2hHIO09dA6ALPE0SPkkWZJGpfBCCI6pvAThTOdcSx+B1bVkKIfiiGegzVGaHkIeM-DaCMuhOGV6p0jATOsNBgJJONbOnS4GFLiwMD7SwWb+R9BBiPTVLMuQ-CPkuisyGIWYHbQFIiH4OvK+TYBTHMKLvDS4xYLa1sInbtqrpMfIcQOGSfCIwWQbGzu4kuHtLqqUDcnLjCbINCsxErKve+rEFazapt628BtG18qfmzV1tJIZYB9VHQ0jWxwdLjoCe+7joQF0XA0lyMbHAPWdEJj9cCoH4-QoYwWYYk4akh7QazFW8jzjLMLpmuXgKh+HB2bPhMdx3TVca8ngLZ-rLCG-QxvZ7bEt57XjF9fxEeiGXQ82uvY+GqfCLnwRB1X6Z1Et23ns6B3Xc96o-dHCDwTIVUeftOST2ngOWeNx5RUyIJ4XYbwB7XDGI0RotBjA6HYPEYYQUNDaGsHqcoog4gQFYOMdA7A0CoAACSCFoCkGhtDPiME5o0OKEAwAxWokAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  library(tern)
  library(formatters)
  ADSL <- tmc_ex_adsl %>%
    mutate(
      DTHFL = case_when(
        !is.na(DTHDT) ~ "Y",
        TRUE ~ ""
      ) %>% with_label("Subject Death Flag")
    )
  ADAE <- tmc_ex_adae

  .add_event_flags <- function(dat) {
    dat <- dat %>%
      mutate(
        TMPFL_SER = AESER == "Y",
        TMPFL_REL = AEREL == "Y",
        TMPFL_GR5 = AETOXGR == "5",
        TMP_SMQ01 = !is.na(SMQ01NAM),
        TMP_SMQ02 = !is.na(SMQ02NAM),
        TMP_CQ01 = !is.na(CQ01NAM)
      )
    column_labels <- list(
      TMPFL_SER = "Serious AE",
      TMPFL_REL = "Related AE",
      TMPFL_GR5 = "Grade 5 AE",
      TMP_SMQ01 = aesi_label(dat[["SMQ01NAM"]], dat[["SMQ01SC"]]),
      TMP_SMQ02 = aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
      TMP_CQ01 = aesi_label(dat[["CQ01NAM"]])
    )
    col_labels(dat)[names(column_labels)] <- as.character(column_labels)
    dat
  }

  #' Generating user-defined event flags.
  ADAE <- ADAE %>% .add_event_flags()

  .ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
  .aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_")]
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_events_summary(
      label = "Adverse Events Summary",
      dataname = "ADAE",
      arm_var = choices_selected(
        choices = variable_choices("ADSL", c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      flag_var_anl = choices_selected(
        choices = variable_choices("ADAE", data[[".ae_anl_vars"]]),
        selected = data[[".ae_anl_vars"]][1],
        keep_order = TRUE,
        fixed = FALSE
      ),
      flag_var_aesi = choices_selected(
        choices = variable_choices("ADAE", data[[".aesi_vars"]]),
        selected = data[[".aesi_vars"]][1],
        keep_order = TRUE,
        fixed = FALSE
      ),
      add_total = TRUE
    )
  )
)
#> Initializing tm_t_events_summary
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
