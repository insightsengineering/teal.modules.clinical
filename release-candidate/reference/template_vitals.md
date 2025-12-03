# Template: Patient Profile Vitals Plot

Creates a valid expression to generate a patient profile vitals
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
plot using ADaM datasets.

## Usage

``` r
template_vitals(
  dataname = "ANL",
  paramcd = "PARAMCD",
  paramcd_levels = c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP"),
  xaxis = "ADY",
  aval_var = "AVAL",
  patient_id,
  font_size = 12L,
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- paramcd_levels:

  (`character`)  
  vector of all levels of `paramcd`.

- xaxis:

  (`character`)  
  name of the time variable to put on the x-axis.

- aval_var:

  (`character`)  
  name of the analysis value variable.

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

[`tm_g_pp_vitals()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_pp_vitals.md)
