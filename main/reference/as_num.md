# Parse text input to numeric vector

Generic to parse text into numeric vectors. This was initially designed
for a robust interpretation of text input in teal modules.

## Usage

``` r
as_num(str)

# Default S3 method
as_num(str)

# S3 method for class 'character'
as_num(str)

# S3 method for class 'numeric'
as_num(str)

# S3 method for class 'factor'
as_num(str)

# S3 method for class 'logical'
as_num(str)
```

## Arguments

- str:

  (`vector`)  
  to extract numeric from.

## Value

As vector of numeric if directly parsed from `numeric` or boolean. A
list of numeric if parsed from a character string, each character string
associated with an list item.

## Details

The function is intended to extract any numeric from a character string,
factor levels, boolean and return a vector of numeric.

## Examples

``` r
dta <- list(
  character = c("text10,20.5letter30.!", "!-.40$$-50e5[", NA),
  factor    = factor(c("]+60e-6, 7.7%%8L", "%90sep.100\"1L", NA_character_)),
  numeric   = c(1, -5e+2, NA),
  logical   = c(TRUE, FALSE, NA)
)
lapply(dta, as_num)
#> $character
#> $character[[1]]
#> [1] 10.0 20.5 30.0
#> 
#> $character[[2]]
#> [1] -4e-01 -5e+06
#> 
#> $character[[3]]
#> [1] NA
#> 
#> 
#> $factor
#> $factor[[1]]
#> [1] 0.00006 7.70000 8.00000
#> 
#> $factor[[2]]
#> [1] 90.0  0.1  1.0
#> 
#> $factor[[3]]
#> [1] NA
#> 
#> 
#> $numeric
#> [1]    1 -500   NA
#> 
#> $logical
#> [1]  1  0 NA
#> 
```
