# Substitute in Quoted Expressions

This version of substitute is needed because
[`substitute()`](https://rdrr.io/r/base/substitute.html) does not
evaluate it's first argument, and it's often useful to be able to modify
a quoted expression.

## Usage

``` r
substitute_q(qexpr, env)
```

## Arguments

- qexpr:

  (`language`)  
  a quoted expression.

- env:

  (`environment` or `list`)  
  requested variable substitutions.

## Value

The modified expression.

## Note

This is simplified from the package `pryr` to avoid another dependency.

## See also

[`substitute_names()`](https://insightsengineering.github.io/teal.modules.clinical/reference/substitute_names.md)
