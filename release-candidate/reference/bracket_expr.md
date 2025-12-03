# Expressions in Brackets

Groups several expressions in a single *bracketed* expression.

## Usage

``` r
bracket_expr(exprs)
```

## Arguments

- exprs:

  (`list` of `call`)  
  expressions to concatenate into a single *bracketed* expression.

## Value

a `{` object. See [`base::Paren()`](https://rdrr.io/r/base/Paren.html)
for details.

## Examples

``` r
adsl <- tmc_ex_adsl
adrs <- tmc_ex_adrs

expr1 <- substitute(
  expr = anl <- subset(df, PARAMCD == param),
  env = list(df = as.name("adrs"), param = "INVET")
)
expr2 <- substitute(expr = anl$rsp_lab <- tern::d_onco_rsp_label(anl$AVALC))
expr3 <- substitute(
  expr = {
    anl$is_rsp <- anl$rsp_lab %in%
      c("Complete Response (CR)", "Partial Response (PR)")
  }
)

res <- bracket_expr(list(expr1, expr2, expr3))
eval(res)
table(anl$rsp_lab, anl$is_rsp)
#>                           
#>                            FALSE TRUE
#>   Complete Response (CR)       0   60
#>   Partial Response (PR)        0   45
#>   Stable Disease (SD)         50    0
#>   Progressive Disease (PD)    39    0
#>   Not Evaluable (NE)           6    0
```
