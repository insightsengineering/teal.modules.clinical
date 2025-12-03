# Convert choices_selected to data_extract_spec with only select_spec

Convert choices_selected to data_extract_spec with only select_spec

## Usage

``` r
cs_to_des_select(
  cs,
  dataname,
  multiple = FALSE,
  ordered = FALSE,
  label = "Select"
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

- ordered:

  (`logical(1)`)  
  Flags whether selection order should be tracked.

- label:

  (`character`)  
  Label to print over the selection field. For no label, set to `NULL`.

## Value

([`teal.transform::data_extract_spec()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html))
