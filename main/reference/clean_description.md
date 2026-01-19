# Clean up categorical variable description

Cleaning categorical variable descriptions before presenting.

## Usage

``` r
clean_description(x)
```

## Arguments

- x:

  (`character`)  
  vector with categories descriptions.

## Value

a string

## Examples

``` r
clean_description("Level A (other text)")
#> [1] "Level A"
clean_description("A long string that should be shortened")
#> [1] "A long string tha..."
```
