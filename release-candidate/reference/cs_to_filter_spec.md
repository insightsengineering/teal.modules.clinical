# Convert choices_selected to filter_spec

Convert choices_selected to filter_spec

## Usage

``` r
cs_to_filter_spec(cs, multiple = FALSE, label = "Filter by")
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

- label:

  (`character`)  
  Label to print over the selection field. For no label, set to `NULL`.

## Value

([`teal.transform::filter_spec()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/filter_spec.html))
