# Wrappers around `srv_transform_teal_data` that allows to decorate the data

Wrappers around `srv_transform_teal_data` that allows to decorate the
data

## Usage

``` r
srv_decorate_teal_data(id, data, decorators, expr, expr_is_reactive = FALSE)

ui_decorate_teal_data(id, decorators, ...)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- expr:

  (`expression` or `reactive`) to evaluate on the output of the
  decoration. When an expression it must be inline code. See
  [`within()`](https://rdrr.io/r/base/with.html) Default is `NULL` which
  won't evaluate any appending code.

- expr_is_reactive:

  (`logical(1)`) whether `expr` is a reactive expression that skips
  defusing the argument.

## Details

`srv_decorate_teal_data` is a wrapper around `srv_transform_teal_data`
that allows to decorate the data with additional expressions. When
original `teal_data` object is in error state, it will show that error
first.

`ui_decorate_teal_data` is a wrapper around `ui_transform_teal_data`.
