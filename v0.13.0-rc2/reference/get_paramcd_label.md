# Extract the associated parameter value for `paramcd`

Utility function for extracting the parameter value that is associated
with the `paramcd` value label. If there is no parameter value for the
`paramcd` label, the `paramcd` value is returned. This is used for
generating the title.

## Usage

``` r
get_paramcd_label(anl, paramcd)
```

## Arguments

- anl:

  Analysis dataset

- paramcd:

  [`teal.transform::data_extract_spec()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html)
  variable value designating the studied parameter.
