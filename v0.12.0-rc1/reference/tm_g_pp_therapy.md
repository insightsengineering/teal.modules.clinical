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

- cmdose:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMDOSE` variable from `dataname`.

- cmtrt:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMTRT` variable from `dataname`.

- cmdosu:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMDOSU` variable from `dataname`.

- cmroute:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMROUTE` variable from `dataname`.

- cmdosfrq:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMDOSFRQ` variable from `dataname`.

- cmstdy:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMSTDY` variable from `dataname`.

- cmendy:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMENDY` variable from `dataname`.

- font_size:

  (`numeric`)  
  numeric vector of length 3 of current, minimum and maximum font size
  values.

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

- ggplot2_args:

  (`ggplot2_args`) optional  
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQ7sxs7Pyo1KyBrhD8UKRQ+kZc1AD68YnWSplJhjoA7rSkABa0EOEJULg6IEo6OgCCACIAwgCyyTqkMARpcAAeaVD8BDD1Tc0AygAyXT19g8P8otQ6AKQAfOsmtNTkjOwOUw4AQgBSAJLNG+U7LR0AJMdnV81BDQ-tjx2NU7bNWydfJfR5-AFAiag35OABygOBRlBjThCKUAF8gkoAFZEcppADWcFYokqiRs+X4cGMUGEpDSBH4tFEfVx+KJJOABHYCjALVmvJqvK+vLkAF0lCMxoTiaIutzef8HM0AJrXQU6XkvC7qvCavn-NF63kdKZOACKGuFtlaAEYrXybQAmB2NG0AZldNoALKKcXiIDKSWSoHJgMK2u1XZHeWKulKYEHRLF+XNKVVgOG+dMZrGJRAvl1cpmIx087E0KguuVijyIA1cjoALw6XK4CZ8IQiOUtzvCMR1hoNHppADmaXQaVKqjQHAmQ501Cg9DgaxbvNsJRnqFYgvnQ9y0Hgzf1Irw+4aqBYFFIR906+zAvP9YXOivGhvDKIa-12ree5fBdIiIekt1oUcSlIE9uQANgABjgmYaidBCkJ0FDULkdtAKHBJaEYVdoJKPECDENJRFXOB1DgfhB1fBoCGImQxBPTQWFoZcRAZJjSNJL4hUdS4sCcXMwCwi8hwokRqP4aCeUE4TRI+V9xJwhiYCpYhZJbRiSLIqSqPIWiJIYniWJbNjGA4+guN05i+MjASOmaJxWgAeWaUVsPohoDJkk8TXaFz3M8sAJNU+ixnKUYiL00RyMomS6Misyex0SzrNs1L2H4-UOkueFWi8kydD8oyArAfLCt5cLvNfMZ+CICjYvshLpKM5L6tS1j2M4uBuLinLHLyoK3LNYq1MkxLyofZyxpcMLJoi+qYFIRgoJ01K2sMmjOoXOzeJ6qy+oG+yho6Jz2nsWwJp80rppoiqOmumqlrq-aNKawQWt47akpKg7zPS3qbP6wGHIukbmjGhxbp8srHp0+S5pOP03oksZGCIQRyB+-SHuMybTLio7MrB7LcsCrA3IcWwFuW+iEe0kbqdphbaoxz7RGMRgAEc8fipm9qHcHSZO8HzvaS7oamAAxLBLTE97XyZp7RrlhXXvohmRZgURSH4VgBb+jqAe6iyQaywbKYNAEVThxmCYq8FVS1lTlfUihDeNoWzZJi3jtB07eMlgSUXhe2lZK1WH3D13Fu1-dlKCIJaGMHR2HKA4oAsbRrBsOoX1EMoIFYRp0HYStHkEWgairijGG0GIIHRJQwHRMUgA)

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
      atirel = choices_selected(
        choices = variable_choices(ADCM, "ATIREL"),
        selected = c("ATIREL")
      ),
      cmdecod = choices_selected(
        choices = variable_choices(ADCM, "CMDECOD"),
        selected = "CMDECOD"
      ),
      cmindc = choices_selected(
        choices = variable_choices(ADCM, "CMINDC"),
        selected = "CMINDC"
      ),
      cmdose = choices_selected(
        choices = variable_choices(ADCM, "CMDOSE"),
        selected = "CMDOSE"
      ),
      cmtrt = choices_selected(
        choices = variable_choices(ADCM, "CMTRT"),
        selected = "CMTRT"
      ),
      cmdosu = choices_selected(
        choices = variable_choices(ADCM, "CMDOSU"),
        selected = c("CMDOSU")
      ),
      cmroute = choices_selected(
        choices = variable_choices(ADCM, "CMROUTE"),
        selected = "CMROUTE"
      ),
      cmdosfrq = choices_selected(
        choices = variable_choices(ADCM, "CMDOSFRQ"),
        selected = "CMDOSFRQ"
      ),
      cmstdy = choices_selected(
        choices = variable_choices(ADCM, "ASTDY"),
        selected = "ASTDY"
      ),
      cmendy = choices_selected(
        choices = variable_choices(ADCM, "AENDY"),
        selected = "AENDY"
      )
    )
  )
)
#> Initializing tm_g_pp_therapy
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
