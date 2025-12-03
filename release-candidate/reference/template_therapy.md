# Template: Patient Profile Therapy Table and Plot

Creates a valid expression to generate a patient profile therapy table
and
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
plot using ADaM datasets.

## Usage

``` r
template_therapy(
  dataname = "ANL",
  atirel = "ATIREL",
  cmdecod = "CMDECOD",
  cmindc = "CMINDC",
  cmdose = "CMDOSE",
  cmtrt = "CMTRT",
  cmdosu = "CMDOSU",
  cmroute = "CMROUTE",
  cmdosfrq = "CMDOSFRQ",
  cmstdy = "CMSTDY",
  cmendy = "CMENDY",
  patient_id,
  font_size = 12L,
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- atirel:

  (`character`)  
  name of time relation of medication variable.

- cmdecod:

  (`character`)  
  name of standardized medication name variable.

- cmindc:

  (`character`)  
  name of indication variable.

- cmdose:

  (`character`)  
  name of dose per administration variable.

- cmtrt:

  (`character`)  
  name of reported name of drug, med, or therapy variable.

- cmdosu:

  (`character`)  
  name of dose units variable.

- cmroute:

  (`character`)  
  name of route of administration variable.

- cmdosfrq:

  (`character`)  
  name of dosing frequency per interval variable.

- cmstdy:

  (`character`)  
  name of study relative day of start of medication variable.

- cmendy:

  (`character`)  
  name of study day of end of medication variable.

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

[`tm_g_pp_therapy()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_pp_therapy.md)
