# Template: Adverse Events Summary

Creates a valid expression to generate an adverse events summary table.

## Usage

``` r
template_events_summary(
  anl_name,
  parentname,
  arm_var,
  dthfl_var = "DTHFL",
  dcsreas_var = "DCSREAS",
  flag_var_anl = NULL,
  flag_var_aesi = NULL,
  aeseq_var = "AESEQ",
  llt = "AEDECOD",
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  count_dth = TRUE,
  count_wd = TRUE,
  count_subj = TRUE,
  count_pt = TRUE,
  count_events = TRUE
)
```

## Arguments

- anl_name:

  (`character`)  
  analysis data used in teal module.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- dthfl_var:

  (`character`)  
  name of variable for subject death flag from `parentname`. Records
  with `"Y"` are summarized in the table row for "Total number of
  deaths".

- dcsreas_var:

  (`character`)  
  name of variable for study discontinuation reason from `parentname`.
  Records with `"ADVERSE EVENTS"` are summarized in the table row for
  "Total number of patients withdrawn from study due to an AE".

- flag_var_anl:

  (`character`)  
  name of flag variable from `dataset` used to count adverse event
  sub-groups (e.g. Serious events, Related events, etc.). Variable
  labels are used as table row names if they exist.

- flag_var_aesi:

  (`character`)  
  name of flag variable from `dataset` used to count adverse event
  special interest groups. All flag variables must be of type `logical`.
  Variable labels are used as table row names if they exist.

- aeseq_var:

  (`character`)  
  name of variable for adverse events sequence number from `dataset`.
  Used for counting total number of events.

- llt:

  (`character`)  
  name of the variable with low level term for events.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- count_dth:

  (`logical`)  
  whether to show count of total deaths (based on `dthfl_var`). Defaults
  to `TRUE`.

- count_wd:

  (`logical`)  
  whether to show count of patients withdrawn from study due to an
  adverse event (based on `dcsreas_var`). Defaults to `TRUE`.

- count_subj:

  (`logical`)  
  whether to show count of unique subjects (based on `USUBJID`). Only
  applies if event flag variables are provided.

- count_pt:

  (`logical`)  
  whether to show count of unique preferred terms (based on `llt`). Only
  applies if event flag variables are provided.

- count_events:

  (`logical`)  
  whether to show count of events (based on `aeseq_var`). Only applies
  if event flag variables are provided.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_events_summary()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_events_summary.md)
