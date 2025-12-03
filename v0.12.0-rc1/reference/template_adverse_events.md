# Template: Patient Profile Adverse Events Table and Plot

Creates a valid expression to generate an adverse events table and
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
plot using ADaM datasets.

## Usage

``` r
template_adverse_events(
  dataname = "ANL",
  aeterm = "AETERM",
  tox_grade = "AETOXGR",
  causality = "AEREL",
  outcome = "AEOUT",
  action = "AEACN",
  time = "ASTDY",
  decod = NULL,
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

- tox_grade:

  (`character`)  
  name of the standard toxicity grade variable.

- causality:

  (`character`)  
  name of the causality variable.

- outcome:

  (`character`)  
  name of outcome of adverse event variable.

- action:

  (`character`)  
  name of action taken with study treatment variable.

- time:

  (`character`)  
  name of study day of start of adverse event variable.

- decod:

  (`character`)  
  name of dictionary derived term variable.

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

[`tm_g_pp_adverse_events()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_pp_adverse_events.md)
