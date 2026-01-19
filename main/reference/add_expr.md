# Expression List

Add a new expression to a list (of expressions).

## Usage

``` r
add_expr(expr_ls, new_expr)
```

## Arguments

- expr_ls:

  (`list` of `call`)  
  the list to which a new expression should be added.

- new_expr:

  (`call`)  
  the new expression to add.

## Value

a `list` of `call`.

## Details

Offers a stricter control to add new expressions to an existing list.
The list of expressions can be later used to generate a pipeline, for
instance with `pipe_expr`.

## Examples

``` r
library(rtables)

lyt <- list()
lyt <- add_expr(lyt, substitute(basic_table()))
lyt <- add_expr(
  lyt, substitute(split_cols_by(var = arm), env = list(armcd = "ARMCD"))
)
lyt <- add_expr(
  lyt,
  substitute(
    test_proportion_diff(
      vars = "rsp", method = "cmh", variables = list(strata = "strata")
    )
  )
)
lyt <- add_expr(lyt, quote(build_table(df = dta)))
pipe_expr(lyt)
#> basic_table() %>% split_cols_by(var = arm) %>% test_proportion_diff(vars = "rsp", 
#>     method = "cmh", variables = list(strata = "strata")) %>% 
#>     build_table(df = dta)
```
