# Substitute Names in a Quoted Expression

This function substitutes the names on both left- and right-hand sides
in a quoted expression. In addition it can also do other standard
substitutions on the right-hand side.

## Usage

``` r
substitute_names(expr, names, others = list())

h_subst_lhs_names(qexpr, names)

substitute_lhs_names(qexpr, names)

substitute_rhs(qexpr, env)
```

## Arguments

- expr:

  (`language`)  
  an expression.

- names:

  (named `list` of `name`)  
  requested name substitutions.

- others:

  (named `list`)  
  requested other substitutions which will only happen on the right-hand
  side.

- qexpr:

  (`language`)  
  a quoted expression.

- env:

  (`environment` or `list`)  
  requested variable substitutions.

## Value

The modified expression.

## Functions

- `h_subst_lhs_names()`: Helper function to just substitute the
  top-level names on the left-hand side in a quoted expression.

- `substitute_lhs_names()`: recursively substitutes all names on the
  left-hand sides in a quoted expression.

- `substitute_rhs()`: substitutes on the right-hand side in a quoted
  expression. Note that this is just a synonym for
  [`substitute_q()`](https://insightsengineering.github.io/teal.modules.clinical/reference/substitute_q.md).

## See also

[`substitute_q()`](https://insightsengineering.github.io/teal.modules.clinical/reference/substitute_q.md)
