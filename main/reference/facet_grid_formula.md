# Facetting formula `x_facet ~ y_facet`

Replaces `x_facet` or `y_facet` by . when empty character

## Usage

``` r
facet_grid_formula(x_facet, y_facet)
```

## Arguments

- x_facet:

  (`character(1)`)  
  name of x facet, if empty, will not facet along x.

- y_facet:

  (`character(1)`)  
  name of y facet, if empty, will not facet along y.

## Value

facet grid formula `formula(x_facet ~ y_facet)`
