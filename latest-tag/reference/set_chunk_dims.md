# Create a reactive that sets plot dimensions on a `teal_card`

This is a convenience function that creates a reactive expression that
automatically sets the `dev.width` and `dev.height` attributes on the
last chunk outputs of a `teal_card` based on plot dimensions from a plot
widget.

## Usage

``` r
set_chunk_dims(pws, q_r, inner_classes = NULL)
```

## Arguments

- pws:

  (`plot_widget`) plot widget that provides dimensions via
  [`dim()`](https://rdrr.io/r/base/dim.html) method

- q_r:

  (`reactive`) reactive expression that returns a `teal_reporter`

- inner_classes:

  (`character`) classes within `chunk_output` that should be modified.
  This can be used to only change `recordedplot`, `ggplot2` or other
  type of objects.

## Value

A reactive expression that returns the `teal_card` with updated
dimensions
