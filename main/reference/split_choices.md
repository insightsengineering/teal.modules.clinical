# Split `choices_selected` objects with interactions into their component variables

Split `choices_selected` objects with interactions into their component
variables

## Usage

``` r
split_choices(x)
```

## Arguments

- x:

  (`choices_selected`)  
  object with interaction terms

## Value

a
[`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
object.

## Note

uses the regex `\\*|:` to perform the split.

## Examples

``` r
split_choices(choices_selected(choices = c("x:y", "a*b"), selected = all_choices()))
#> $choices
#> [1] "x" "y" "a" "b"
#> 
#> $selected
#> [1] "x" "y" "a" "b"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
```
