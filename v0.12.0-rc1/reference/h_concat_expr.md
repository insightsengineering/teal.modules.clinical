# Expression Deparsing

Deparse an expression into a `string`.

## Usage

``` r
h_concat_expr(expr)
```

## Arguments

- expr:

  (`call`)  
  or an object which can be used as so.

## Value

a `string`.

## Examples

``` r
expr <- quote({
  library(rtables)
  basic_table() %>%
    split_cols_by(var = "ARMCD") %>%
    test_proportion_diff(
      vars = "rsp", method = "cmh", variables = list(strata = "strata")
    ) %>%
    build_table(df = dta)
})

h_concat_expr(expr)
#> [1] "{\n    library(rtables)\n    basic_table() %>% split_cols_by(var = \"ARMCD\") %>% test_proportion_diff(vars = \"rsp\", \n        method = \"cmh\", variables = list(strata = \"strata\")) %>% \n        build_table(df = dta)\n}"
```
