# Template: Patient Profile Timeline Plot

Creates a valid expression to generate a patient profile timeline
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
plot using ADaM datasets.

## Usage

``` r
template_patient_timeline(
  dataname = "ANL",
  aeterm = "AETERM",
  aetime_start = "ASTDTM",
  aetime_end = "AENDTM",
  dstime_start = "CMASTDTM",
  dstime_end = "CMAENDTM",
  cmdecod = "CMDECOD",
  aerelday_start = NULL,
  aerelday_end = NULL,
  dsrelday_start = NULL,
  dsrelday_end = NULL,
  relative_day = FALSE,
  patient_id,
  font_size = 12L,
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- aeterm:

  (`character`)  
  name of the reported term for the adverse event variable.

- aetime_start:

  (`character`)  
  name of start date/time of adverse event variable.

- aetime_end:

  (`character`)  
  name of end date/time of adverse event variable.

- dstime_start:

  (`character`)  
  name of date/time of first exposure to treatment variable.

- dstime_end:

  (`character`)  
  name of date/time of last exposure to treatment variable.

- cmdecod:

  (`character`)  
  name of standardized medication name variable.

- aerelday_start:

  (`character`)  
  name of adverse event study start day variable.

- aerelday_end:

  (`character`)  
  name of adverse event study end day variable.

- dsrelday_start:

  (`character`)  
  name of concomitant medications study start day variable.

- dsrelday_end:

  (`character`)  
  name of concomitant medications study day start variable.

- relative_day:

  (`logical`)  
  whether to use relative days (`TRUE`) or absolute dates (`FALSE`).

- patient_id:

  (`character`)  
  patient ID.

- font_size:

  (`numeric`)  
  font size value.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_g_pp_patient_timeline()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_pp_patient_timeline.md)
