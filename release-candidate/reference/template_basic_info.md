# Template: Patient Profile Basic Info

Creates a valid expression to generate a patient profile basic info
report using ADaM datasets.

## Usage

``` r
template_basic_info(dataname = "ANL", vars, patient_id = NULL)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- vars:

  (`character`)  
  names of the variables to be shown in the table.

- patient_id:

  (`character`)  
  patient ID.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_pp_basic_info()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_pp_basic_info.md)
