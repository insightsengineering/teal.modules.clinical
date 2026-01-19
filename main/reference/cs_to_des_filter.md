# Convert choices_selected to data_extract_spec with only filter_spec

Convert choices_selected to data_extract_spec with only filter_spec

## Usage

``` r
cs_to_des_filter(
  cs,
  dataname,
  multiple = FALSE,
  include_vars = FALSE,
  label = "Filter by"
)
```

## Arguments

- cs:

  (`choices_selected`)  
  object to be transformed. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details.

- dataname:

  (`character`)  
  name of the data

- multiple:

  (`logical`)  
  Whether multiple values shall be allowed in the shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

- include_vars:

  (`flag`)  
  whether to include the filter variables as fixed selection in the
  result. This can be useful for preserving for reuse in `rtables` code
  e.g.

- label:

  (`character`)  
  Label to print over the selection field. For no label, set to `NULL`.

## Value

([`teal.transform::data_extract_spec()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html))
