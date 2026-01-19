# Convert choices_selected to select_spec

Convert choices_selected to select_spec

## Usage

``` r
cs_to_select_spec(cs, multiple = FALSE, ordered = FALSE, label = "Select")
```

## Arguments

- cs:

  (`choices_selected`)  
  object to be transformed. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details.

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

(`select_spec`)
