# Subset decorators based on the scope

Subset decorators based on the scope

## Usage

``` r
select_decorators(decorators, scope)
```

## Arguments

- decorators:

  (named `list`) of list decorators to subset.

- scope:

  (`character`) a character vector of decorator names to include.

## Value

Subsetted list with all decorators to include. It can be an empty list
if none of the scope exists in `decorators` argument.
