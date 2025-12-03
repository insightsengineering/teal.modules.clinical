# Split interaction terms into their component variables

Split interaction terms into their component variables

## Usage

``` r
split_interactions(x, by = "\\*|:")
```

## Arguments

- x:

  (`character`)  
  string representing the interaction usually in the form `x:y` or
  `x*y`.

- by:

  (`character`)  
  regex with which to split the interaction term by.

## Value

a vector of strings where each element is a component variable extracted
from interaction term `x`.

## Examples

``` r
split_interactions("x:y")
#> [1] "x" "y"
split_interactions("x*y")
#> [1] "x" "y"
```
