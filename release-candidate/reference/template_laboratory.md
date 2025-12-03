# Template: Patient Profile Laboratory Table

Creates a valid expression to generate a patient profile laboratory
table using ADaM datasets.

## Usage

``` r
template_laboratory(
  dataname = "ANL",
  paramcd = "PARAMCD",
  param = "PARAM",
  anrind = "ANRIND",
  timepoints = "ADY",
  aval_var = "AVAL",
  avalu_var = "AVALU",
  patient_id = NULL,
  round_value = 0L
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- param:

  (`character`)  
  name of the parameter variable.

- anrind:

  (`character`)  
  name of the analysis reference range indicator variable.

- timepoints:

  (`character`)  
  name of time variable.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- avalu_var:

  (`character`)  
  name of the analysis value unit variable.

- patient_id:

  (`character`)  
  patient ID.

- round_value:

  (`numeric`)  
  number of decimal places to round to.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_pp_laboratory()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_pp_laboratory.md)
