# Flatten draggable bucket contents to non-empty character arms

Aligns with
[`teal::validate_has_elements()`](https://insightsengineering.github.io/teal/latest-tag/reference/validate_has_elements.html)
used by
[`validate_standard_inputs()`](https://insightsengineering.github.io/teal.modules.clinical/reference/validate_standard_inputs.md):
list length can be positive while
[`unlist()`](https://rdrr.io/r/base/unlist.html) is empty (e.g. nested
empty vectors).

## Usage

``` r
arm_bucket_values(buckets, name)
```

## Arguments

- buckets:

  (`list` or `NULL`)\
  value of Shiny `input$buckets`.

- name:

  (`character(1)`)\
  bucket id, typically `"Ref"` or `"Comp"`.

## Value

[`character()`](https://rdrr.io/r/base/character.html) (possibly length
zero).
