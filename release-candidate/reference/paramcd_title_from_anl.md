# Title label for parameter code(s) from analysis data (picks-based modules)

Title label for parameter code(s) from analysis data (picks-based
modules)

## Usage

``` r
paramcd_title_from_anl(anl, paramcd_name, paramcd_vals)
```

## Arguments

- anl:

  (`data.frame`)\
  analysis dataset after filtering.

- paramcd_name:

  (`character(1)`)\
  column name for parameter code (e.g. `PARAMCD`).

- paramcd_vals:

  (`character`)\
  selected parameter code value(s).

## Value

A single string suitable for table titles.
