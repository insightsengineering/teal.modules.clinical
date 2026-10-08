# Validate standard input values for a teal module

Validates standard input.

## Usage

``` r
validate_standard_inputs(
  adsl,
  adslvars = character(0),
  anl,
  anlvars = character(0),
  need_arm = TRUE,
  arm_var,
  ref_arm,
  comp_arm,
  min_n_levels_armvar = 1L,
  max_n_levels_armvar = 100L,
  min_nrow = 1
)
```

## Arguments

- adsl:

  data.frame with subject-level data

- adslvars:

  required variables from `ADSL`

- anl:

  data.frame with analysis data

- anlvars:

  required variables from `ANL`

- need_arm:

  flag indicating whether grouping variable `arm_var` is required or can
  be optionally `NULL`.

- arm_var:

  character with name of grouping variable, typically arm

- ref_arm:

  character with name of reference level in `arm_var`

- comp_arm:

  character with name for comparison level in `arm_var`

- min_n_levels_armvar:

  minimum number of levels in grouping variable `arm_var`. Defaults to
  1, `NULL` for no minimum.

- max_n_levels_armvar:

  maximum number of levels in grouping variable `arm_var`. Use `NULL`
  for no maximum.

- min_nrow:

  minimum number of observations in `ADSL` and `ANL`
