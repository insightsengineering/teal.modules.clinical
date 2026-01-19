# Expression: Prepare Arm Levels

This function generates the standard expression for pre-processing of
dataset arm levels in and is used to apply the same steps in safety teal
modules.

## Usage

``` r
prepare_arm_levels(dataname, parentname, arm_var, drop_arm_levels = TRUE)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

## Value

a `{` object. See [`base::Paren()`](https://rdrr.io/r/base/Paren.html)
for details.

## Examples

``` r
prepare_arm_levels(
  dataname = "adae",
  parentname = "adsl",
  arm_var = "ARMCD",
  drop_arm_levels = TRUE
)
#> {
#>     adae <- adae %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
#>     arm_levels <- levels(adae[["ARMCD"]])
#>     adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
#>     adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
#> }

prepare_arm_levels(
  dataname = "adae",
  parentname = "adsl",
  arm_var = "ARMCD",
  drop_arm_levels = FALSE
)
#> {
#>     adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
#>     arm_levels <- levels(adsl[["ARMCD"]])
#>     adae <- adae %>% dplyr::mutate(ARMCD = factor(ARMCD, levels = arm_levels))
#> }
```
