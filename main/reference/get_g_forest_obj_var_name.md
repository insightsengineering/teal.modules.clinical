# Utility function for extracting `paramcd` for forest plots

Utility function for extracting `paramcd` for forest plots

## Usage

``` r
get_g_forest_obj_var_name(paramcd, input, filter_idx = 1)
```

## Arguments

- paramcd:

  [`teal.transform::data_extract_spec()`](https://rdrr.io/pkg/teal.transform/man/data_extract_spec.html)
  variable value designating the studied parameter.

- input:

  shiny app input

- filter_idx:

  filter section index (default 1)
