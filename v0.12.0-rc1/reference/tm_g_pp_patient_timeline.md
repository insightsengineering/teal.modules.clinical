# teal Module: Patient Profile Timeline Plot

This module produces a patient profile timeline
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type plot using ADaM datasets.

## Usage

``` r
tm_g_pp_patient_timeline(
  label,
  dataname_adcm = "ADCM",
  dataname_adae = "ADAE",
  parentname = "ADSL",
  patient_col = "USUBJID",
  aeterm = NULL,
  cmdecod = NULL,
  aetime_start = NULL,
  aetime_end = NULL,
  dstime_start = NULL,
  dstime_end = NULL,
  aerelday_start = NULL,
  aerelday_end = NULL,
  dsrelday_start = NULL,
  dsrelday_end = NULL,
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

- dataname_adcm:

  (`character`)  
  name of `ADCM` dataset or equivalent.

- dataname_adae:

  (`character`)  
  name of `ADAE` dataset or equivalent.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- patient_col:

  (`character`)  
  name of patient ID variable.

- aeterm:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AETERM` variable from `dataname`.

- cmdecod:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMDECOD` variable from `dataname_adcm`.

- aetime_start:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `ASTDTM` variable from `dataname_adae`.

- aetime_end:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AENDTM` variable from `dataname_adae`.

- dstime_start:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMASTDTM` variable from `dataname_adcm`.

- dstime_end:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `CMAENDTM` variable from `dataname_adcm`.

- aerelday_start:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `ASTDY` variable from `dataname_adae`.

- aerelday_end:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AENDY` variable from `dataname_adae`.

- dsrelday_start:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `ASTDY` variable from `dataname_adcm`.

- dsrelday_end:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AENDY` variable from `dataname_adcm`.

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
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_pp_patient_timeline(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHMYBMGHkjKJBZRWe+ajUrIFtAIIAItNOkTqkMAQxcAAeMVChcDOzAMoAMkvGtNT9nKvrWzui1EUOBw4AQgBSAJKzOgCkuT86OYLAAkT1en1mQ0BswAwgBZJYrNabbb8AgwX4APh+bTKMEE4XI7FxZR08IOtlmAE0dABeHSyURwGJpLIUYkQUlcslwmHTWx0+kKMDwNHUHQvYU6AB+OgATAAGXAkrnwvkC2lCkVwMVkqWygCMAFYlSrSfZnDKdAazTYftj0pksjFqFB6HBqMSwAdSIJ+KwdLMoAGiMYdD6WKQdKGdHCdS0NCRhXJlZzufCnAA5al0hlQJkstl5M1lNX8wU6YWiyQS-U6ADsCtNae5PPVFarOprMLrBqbzdbZQti1lJoVtqxAIy2Rdbo9Xp9foDQZDYacEH40bDcf4CcswTAKbN8OmFNmtgR9NPlIvqdbJ6z58vgMfF5JQQAvkElAArIi5GIAGs4FYUR8mSGxUn4OBjCgYRSBiAhd1ENY-wA4DQOAAgvTmY5hSKYUgRcPBKzAOZ4WTABdJQdnRICQNEJZsOFCkHGpL58NIsF3g4kjCLPN8+LAcknAARU4wjbBhG0hP5GE5QksipIAZkUuSABZk1-f8IHo0DwPCORgEI2E4TU0zhUopZaJgPTRG09CGIMqAjJMhZzIosArNSZjvVsNiqV4giwG4iEtOCaB0CWXJMg5MoClzAo7x0PghBERj6VShowJJFYYgAcxidAirCWgKAQjR4CkOA4u5V13XFLUAAVSvKuxaCq3I9jwM0CmgeBUSgXQtSI-DeuSfrmRs3MTM85KuVQFhysmmayMOI4xpbUlFo0crEKIRquOeHjZk21txiIBC2VofKsijelsIANibI4ikVBVXvlfsjiPLaynRaDiE3B6sn-AgxBiJkRHUHVatbAhQZkMRc00FhaDdEREMR8GwIKYBjLWzzKMo4L4VmJwYQAeVOw95u5KG4Bh4HSLJinqbO7lftbIb+gxEGwYhhmmbh7kEYFjKdFRxh0foTGxaR3HknxtziOJ4KFlsJwsDM2mSx0IXyGZ3yNa1nWoS5LnuR5jrmVEcJGHuhlscFj1GcNkWuXlnGUbRjHmS9sRnOVtb3K8knSOvJ9kzprkDZ1XNjYEs3bRjnRrYGigjed0RIdd4W9YDiWpZluXs6DgnRrD9XXzN1Oyjjo2cJr8LW0trl+Dtm3Iftx3C9z6H3YL7Ofelv2sfF8vZp1tWWbhSPBLb+m88NhOvRPJOW851OO8q5lM4T7P+7d2Gh-FkeS-9su8Yriyq9nhZswXuv9eX+OHrXufm8PFOzSGxgPVCKwbukYD7iyPvnP6pJC7nzHoXSeIdVbh34pSKk0c9YN1Xsg6km8Lapz-gA4M6wNygIVuAwekD-rD3pMXWBV8lY31DjPQij5UG6woS-Aeb8GRN2zKw82pJF7xVEP-aggDgEOxITjMhJ92HQOob7WWl8J7XynpZJBZEzx8Ofhg9+WC+E-0gR3ERYj9781IQ3D2UCqGSwUaXZR9DVF32YbwtB7CdHcOcdg7+kD+FQiCEEWgYZ2C5H6FACw2hrA2BKGmUQOQICsGmOgdgaBUDAkELQIoKTgRMkYNoKYEAPxKDAB+SiQA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(formatters)
  library(dplyr)
  ADAE <- tmc_ex_adae
  ADSL <- filter(tmc_ex_adsl, USUBJID %in% ADAE$USUBJID)
  ADCM <- tmc_ex_adcm %>%
    mutate(
      CMSTDY = case_when(
        CMCAT == "medcl B" ~ 20,
        CMCAT == "medcl C" ~ 150,
        TRUE ~ 1
      ) %>% with_label("Study Day of Start of Medication"),
      CMENDY = case_when(
        CMCAT == "medcl B" ~ 700,
        CMCAT == "medcl C" ~ 1000,
        TRUE ~ 500
      ) %>% with_label("Study Day of End of Medication"),
      CMASTDTM = ASTDTM,
      CMAENDTM = AENDTM
    )
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE", "ADCM")]
adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
join_keys(data)["ADCM", "ADCM"] <- adcm_keys
join_keys(data)["ADAE", "ADCM"] <- c("STUDYID", "USUBJID")

app <- init(
  data = data,
  modules = modules(
    tm_g_pp_patient_timeline(
      label = "Patient Timeline",
      dataname_adae = "ADAE",
      dataname_adcm = "ADCM",
      parentname = "ADSL",
      patient_col = "USUBJID",
      plot_height = c(600L, 200L, 2000L),
      cmdecod = choices_selected(
        choices = variable_choices(data[["ADCM"]], "CMDECOD"),
        selected = "CMDECOD",
      ),
      aeterm = choices_selected(
        choices = variable_choices(data[["ADAE"]], "AETERM"),
        selected = c("AETERM")
      ),
      aetime_start = choices_selected(
        choices = variable_choices(data[["ADAE"]], "ASTDTM"),
        selected = c("ASTDTM")
      ),
      aetime_end = choices_selected(
        choices = variable_choices(data[["ADAE"]], "AENDTM"),
        selected = c("AENDTM")
      ),
      dstime_start = choices_selected(
        choices = variable_choices(data[["ADCM"]], "CMASTDTM"),
        selected = c("CMASTDTM")
      ),
      dstime_end = choices_selected(
        choices = variable_choices(data[["ADCM"]], "CMAENDTM"),
        selected = c("CMAENDTM")
      ),
      aerelday_start = choices_selected(
        choices = variable_choices(data[["ADAE"]], "ASTDY"),
        selected = c("ASTDY")
      ),
      aerelday_end = choices_selected(
        choices = variable_choices(data[["ADAE"]], "AENDY"),
        selected = c("AENDY")
      ),
      dsrelday_start = choices_selected(
        choices = variable_choices(data[["ADCM"]], "ASTDY"),
        selected = c("ASTDY")
      ),
      dsrelday_end = choices_selected(
        choices = variable_choices(data[["ADCM"]], "AENDY"),
        selected = c("AENDY")
      )
    )
  )
)
#> Initializing tm_g_pp_patient_timeline
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
