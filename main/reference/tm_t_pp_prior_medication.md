# teal Module: Patient Profile Prior Medication

This module produces a patient profile prior medication report using
ADaM datasets.

## Usage

``` r
tm_t_pp_prior_medication(
  label,
  dataname = "ADCM",
  parentname = "ADSL",
  patient_col = "USUBJID",
  atirel = NULL,
  cmdecod = NULL,
  cmindc = NULL,
  cmstdy = NULL,
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

- atirel:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `ATIREL` variable from `dataname`.

- cmdecod:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMDECOD` variable from `dataname`.

- cmindc:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMINDC` variable from `dataname`.

- cmstdy:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMSTDY` variable from `dataname`.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEPxQpFD6RlzUAPpBIdZKMaGGOgDutKQAFrQQ7Am4OiBKOjoAggAiAMIAsmE6pDAEkXAAHpFQ-AQwhSWlAMoAMjV1Dc2t-KLUOgCkAHyTJrTU5IzsDj0OAEIAUgCSpVNZc2VVACSrGzulAUVHlcdVxT22pbbVSTfHD08vXe-3TgByz1eRnexQBQKUAF8AgArIhZSIAazgrFEOWCUBsSX4cGMUGEpEiBH4tFEDThCORqOA0HgaIScgAuko2h0kSjRDUCOwFGBHg5SgBNXa8vK8s5bEV4HS8z4Q6W8qo9JwARVFMrAxVs5QAjOrZdqAEz6zXagDMJq15QALLzYfCIOzUeiQnJgLKKpVLZ7eYyaqyYE7RK4IGV+jUEsB3Zren1fczQ56Ixiox6qvGQ2hUDUsmkeRAigkdABeHS5Lp8IQiTmlyvCMT5opFOqRQnoSKoRiWRiReAk2QaKxdJs6ahQehwCal3kABS7REYOkqcH7wUsEFFw6bCVpumnMfTeC3RVQLAopF3JY1YbjR4LI50p4056JRCnGolF0395Ha8Yk6vAh0nhAgxEiURJzgdQV0bB8iiAkCxCvTQWFoccRCJYCZAbG4xVNbYsCcW85FwY8mwgkRoP4K8DQIojeTIkiyI6HFiGo0sEOw0RwMgqjYLgzjQJrHQUK7dC4EwxC0VwjUqlKJxygAeVKO1SJ-B8KKg8h2Nkyp5KUlSwEYtSBJgLJ2kArChJ4yjtP4h9BKQ0tRLQ+gMMc6TPTwqptkBcpVLIopNKomiwB8vyGPUoomKinQOlEUh+FYSypJsrSYMCuKrKckTUPEySuPYGTZUeIUAtioLeO00K5UFSK4KuJtGoCAJaGMHR2CyJYoAsbRrBsAp71ETIIFYYp0HYLNjkEWg8imiDGG0fwoSUMBIUZIA)

## Examples

``` r
library(dplyr)
data <- teal_data()
data <- within(data, {
  ADCM <- tmc_ex_adcm
  ADSL <- tmc_ex_adsl %>% filter(USUBJID %in% ADCM$USUBJID)
  ADCM$CMASTDTM <- ADCM$ASTDTM
  ADCM$CMAENDTM <- ADCM$AENDTM
})
join_keys(data) <- default_cdisc_join_keys[names(data)]
adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
join_keys(data)["ADCM", "ADCM"] <- adcm_keys

ADSL <- data[["ADSL"]]
ADCM <- data[["ADCM"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_pp_prior_medication(
      label = "Prior Medication",
      dataname = "ADCM",
      parentname = "ADSL",
      patient_col = "USUBJID",
      atirel = choices_selected(
        choices = variable_choices(ADCM, "ATIREL"),
        selected = "ATIREL"
      ),
      cmdecod = choices_selected(
        choices = variable_choices(ADCM, "CMDECOD"),
        selected = "CMDECOD"
      ),
      cmindc = choices_selected(
        choices = variable_choices(ADCM, "CMINDC"),
        selected = "CMINDC"
      ),
      cmstdy = choices_selected(
        choices = variable_choices(ADCM, "ASTDY"),
        selected = "ASTDY"
      )
    )
  )
)
#> Initializing tm_t_pp_prior_medication
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
