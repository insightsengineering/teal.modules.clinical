# Template: Patient Profile Medical History

Creates a valid expression to generate a patient profile medical history
report using ADaM datasets.

## Usage

``` r
template_medical_history(
  dataname = "ANL",
  mhterm = "MHTERM",
  mhbodsys = "MHBODSYS",
  mhdistat = "MHDISTAT",
  patient_id = NULL
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- mhterm:

  (`character`)  
  name of the reported term for the medical history variable.

- mhbodsys:

  (`character`)  
  name of the body system or organ class variable.

- mhdistat:

  (`character`)  
  name of the status of the disease variable.

- patient_id:

  (`character`)  
  patient ID.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_pp_medical_history()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_pp_medical_history.md)
