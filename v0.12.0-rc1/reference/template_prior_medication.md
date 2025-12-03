# Template: Patient Profile Prior Medication

Creates a valid expression to generate a patient profile prior
medication report using ADaM datasets.

## Usage

``` r
template_prior_medication(
  dataname = "ANL",
  atirel = "ATIREL",
  cmdecod = "CMDECOD",
  cmindc = "CMINDC",
  cmstdy = "CMSTDY"
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

- cmstdy:

  (`character`)  
  name of study relative day of start of medication variable.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_pp_prior_medication()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_pp_prior_medication.md)
