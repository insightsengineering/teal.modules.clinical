# Expressions as a Pipeline

Concatenate expressions in a single pipeline-flavor expression.

## Usage

``` r
pipe_expr(exprs, pipe_str = "%>%")
```

## Arguments

- exprs:

  (`list` of `call`)  
  expressions to concatenate in a pipeline (`%>%`).

- pipe_str:

  (`character`)  
  the character which separates the expressions.

## Value

a `call`

## Examples

``` r
pipe_expr(
  list(
    expr1 = substitute(df),
    expr2 = substitute(head)
  )
)
#> df %>% head
```
