# Split-Column Expression

Renders the expression for column split in `rtables` depending on:

- the expected or not arm comparison

- the expected or not arm combination

## Usage

``` r
split_col_expr(compare, combine, ref, arm_var)
```

## Arguments

- compare:

  (`logical`)  
  if `TRUE` the reference level is included.

- combine:

  (`logical`)  
  if `TRUE` the group combination is included.

- ref:

  (`character`)  
  the reference level (not used for `combine = TRUE`).

- arm_var:

  (`character`)  
  the arm or grouping variable name.

## Value

a `call`

## Examples

``` r
split_col_expr(
  compare = TRUE,
  combine = FALSE,
  ref = "ARM A",
  arm_var = "ARMCD"
)
#> rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A")
```
