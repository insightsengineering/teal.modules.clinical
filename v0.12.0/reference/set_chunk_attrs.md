# Set the attributes of the last chunk outputs

This function modifies the attributes of the last `n` elements of a
`teal_card` that are `chunk_output` objects. It can be used to set
attributes like `dev.width` and `dev.height` for plot outputs.

## Usage

``` r
set_chunk_attrs(
  teal_card,
  attributes,
  n = 1,
  inner_classes = NULL,
  quiet = FALSE
)
```

## Arguments

- teal_card:

  (`teal_card`) the teal_card object to modify

- attributes:

  (`list`) named list of attributes to set

- n:

  (`integer(1)`) number of the last element of `teal_card` to modify. it
  will only change `chunk_output` objects.

- inner_classes:

  (`character`) classes within `chunk_output` that should be modified.
  This can be used to only change `recordedplot`, `ggplot2` or other
  type of objects.

- quiet:

  (`logical`) whether to suppress warnings
