# Concatenate expressions via a binary operator

e.g. combine with `+` for `ggplot` without introducing parentheses due
to associativity

## Usage

``` r
call_concatenate(args, bin_op = "+")
```

## Arguments

- args:

  arguments to concatenate with operator

- bin_op:

  binary operator to concatenate it with

## Value

a `call`

## Examples

``` r
library(ggplot2)

# What we want to achieve
call("+", quote(f), quote(g))
#> f + g
call("+", quote(f), call("+", quote(g), quote(h))) # parentheses not wanted
#> f + (g + h)
call("+", call("+", quote(f), quote(g)), quote(h)) # as expected without unnecessary parentheses
#> f + g + h
Reduce(function(existing, new) call("+", existing, new), list(quote(f), quote(g), quote(h)))
#> f + g + h

# how we do it
call_concatenate(list(quote(f), quote(g), quote(h)))
#> f + g + h
call_concatenate(list(quote(f)))
#> f
call_concatenate(list())
#> NULL
call_concatenate(
  list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg))))
)
#> ggplot(mtcars) + geom_point(aes(wt, mpg))

eval(
  call_concatenate(
    list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg))))
  )
)

```
