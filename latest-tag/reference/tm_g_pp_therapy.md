# teal Module: Patient Profile Therapy Table and Plot

This module produces a patient profile therapy table and
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type plot using ADaM datasets.

## Usage

``` r
tm_g_pp_therapy(
  label,
  dataname = "ADCM",
  parentname = "ADSL",
  patient_col = "USUBJID",
  atirel = NULL,
  cmdecod = NULL,
  cmindc = NULL,
  cmdose = NULL,
  cmtrt = NULL,
  cmdosu = NULL,
  cmroute = NULL,
  cmdosfrq = NULL,
  cmstdy = NULL,
  cmendy = NULL,
  font_size = c(12L, 12L, 25L),
  plot_height = c(700L, 200L, 2000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
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

- patient_col:

  (`character`)\
  name of patient ID variable.

- atirel:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `ATIREL` variable from `dataname`.

- cmdecod:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMDECOD` variable from `dataname`.

- cmindc:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMINDC` variable from `dataname`.

- cmdose:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMDOSE` variable from `dataname`.

- cmtrt:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMTRT` variable from `dataname`.

- cmdosu:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMDOSU` variable from `dataname`.

- cmroute:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMROUTE` variable from `dataname`.

- cmdosfrq:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMDOSFRQ` variable from `dataname`.

- cmstdy:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMSTDY` variable from `dataname`.

- cmendy:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  `CMENDY` variable from `dataname`.

- font_size:

  (`numeric`)\
  numeric vector of length 3 of current, minimum and maximum font size
  values.

- plot_height:

  (`numeric`) optional\
  vector of length three with `c(value, min, max)`. Specifies the height
  of the main plot and renders a slider on the plot to interactively
  adjust the plot height.

- plot_width:

  (`numeric`) optional\
  vector of length three with `c(value, min, max)`. Specifies the width
  of the main plot and renders a slider on the plot to interactively
  adjust the plot width.

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

- ggplot2_args:

  (`ggplot2_args`) optional\
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
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

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators::

- `plot` (`ggplot`) A Decorator is applied to the specific output using
  a named list of `teal_transform_module` objects. The name of this list
  corresponds to the name of the output to which the decorator is
  applied. See code snippet below:

    tm_g_pp_therapy(
       ..., # arguments for module
       decorators = list(
         plot = teal_transform_module(...) # applied only to `plot` output
       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.modules.clinical")`](https://insightsengineering.github.io/teal.modules.clinical/articles/decorate-module-output.md).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQ7sxs7Pyo1KyBrhD8UKRQ+kZc1AD68YnWSplJhjoA7rSkABa0EOEJULg6IEo6OgCCACIAwgCyyTqkMARpcAAeaVD8BDD1Tc0AygAyXT19g8P8otQ6AKQAfOsmtNTkjOwOUw4AQgBSAJLNG+U7LR0AJMdnV81BDQ-tjx2NU7bNWydfJfR5-AFAiag35OABygOBRlBjThCKUAF8gkoAFZEcppADWcFYokqiRs+X4cGMUGEpDSBH4tFEfVx+KJJOABHYCjALVmvJqvK+vLkAF0lCMxoTiaIutzef8HM0AJrXQU6XkvC7qvCavn-NF63kdKZOACKGuFtlaAEYrXybQAmB2NG0AZldNoALKKcXiIDKSWSoHJgMK2u1XZHeWKulKYEHRLF+XNKVVgOG+dMZrGJRAvl1cpmIx087E0KguuVijyIA1cjoALw6XK4CZ8IQiOUtzvCMR1hoNHppADmaXQaVKqjQHAmQ501Cg9DgaxbvNsJRnqFYgvnQ9y0Hgzf1Irw+4aqBYFFIR906+zAvP9YXOivGhvDKIa-12ree5fBdIiIekt1oUcSlIE9uQANgABjgmYaidBCkJ0FDULkdtAKHBJaEYVcT00FhaGXbt2AIEo8QIMQT2tS4sCcXM9VEVc4HUOB+Dox0GKY0VsNfHQxipYguJbYjGFI+hyMo6jaIfDpmicVoAHlmg1ViRA4sT9UU5S1P4i8hJgcpRiIkiyIHWSZHk3T2kueFWg0tjtO4joHLaQycIaYSiFY8zJMs0lrJons7OaFSzWcrTyB0k12giqKwCwoyxlIRgoPEizpKsqibLC+L7FsaL2Nitz2iKrzBN80RBACqSZLy0LysShwStchSEsitrkoE18xkYIhBHIeqgooprbPirAVIcWwXBYlyys66bZvmlLvOM-g-OMRgAEdRpy4KJoKsBFMigAxLBLQWmLOJai6rqq-qYFEUh+FYA7Grkk7wVVdqltPQ0VSehcxgod7Pty77uJReFgZu0q7ofWG-uS-cPh0IIgloYwdHYcoDigCxtGsGw6hfUQyggVhGnQdhK0eQRaBqBnWMYbQYggdElDAdExSAA)

## Examples

``` r
library(nestcolor)
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADCM <- tmc_ex_adcm
  ADSL <- tmc_ex_adsl %>% filter(USUBJID %in% ADCM$USUBJID)
  ADCM$CMASTDTM <- ADCM$ASTDTM
  ADCM$CMAENDTM <- ADCM$AENDTM
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADCM")]
adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
join_keys(data)["ADCM", "ADCM"] <- adcm_keys

ADSL <- data[["ADSL"]]
ADCM <- data[["ADCM"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_pp_therapy(
      label = "Therapy",
      dataname = "ADCM",
      parentname = "ADSL",
      patient_col = "USUBJID",
      plot_height = c(600L, 200L, 2000L),
      atirel = variables(choices = "ATIREL", selected = "ATIREL"),
      cmdecod = variables(choices = "CMDECOD", selected = "CMDECOD"),
      cmindc = variables(choices = "CMINDC", selected = "CMINDC"),
      cmdose = variables(choices = "CMDOSE", selected = "CMDOSE"),
      cmtrt = variables(choices = "CMTRT", selected = "CMTRT"),
      cmdosu = variables(choices = "CMDOSU", selected = "CMDOSU"),
      cmroute = variables(choices = "CMROUTE", selected = "CMROUTE"),
      cmdosfrq = variables(choices = "CMDOSFRQ", selected = "CMDOSFRQ"),
      cmstdy = variables(choices = "ASTDY", selected = "ASTDY"),
      cmendy = variables(choices = "AENDY", selected = "AENDY")
    )
  )
)
#> Initializing tm_g_pp_therapy
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
