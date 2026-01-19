# Expression: Arm Preparation

The function generate the standard expression for pre-processing of
dataset in teal module applications. This is especially of interest when
the same preprocessing steps needs to be applied similarly to several
datasets (e.g. `ADSL` and `ADRS`).

## Usage

``` r
prepare_arm(
  dataname,
  arm_var,
  ref_arm,
  comp_arm,
  compare_arm = !is.null(ref_arm),
  ref_arm_val = paste(ref_arm, collapse = "/"),
  drop = TRUE
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- compare_arm:

  (`logical`)  
  triggers the comparison between study arms.

- ref_arm_val:

  (`character`)  
  replacement name for the reference level.

- drop:

  (`logical`)  
  drop the unused variable levels.

## Value

a `call`

## Details

In `teal.modules.clinical`, the user interface includes manipulation of
the study arms. Classically: the arm variable itself (e.g. `ARM`,
`ACTARM`), the reference arm (0 or more), the comparison arm (1 or more)
and the possibility to combine comparison arms.

Note that when no arms should be compared with each other, then the
produced expression is reduced to optionally dropping non-represented
levels of the arm.

When comparing arms, the pre-processing includes three steps:

1.  Filtering of the dataset to retain only the arms of interest
    (reference and comparison).

2.  Optional, if more than one arm is designated as *reference* they are
    combined into a single level.

3.  The reference is explicitly reassigned and the non-represented
    levels of arm are dropped.

## Examples

``` r
prepare_arm(
  dataname = "adrs",
  arm_var = "ARMCD",
  ref_arm = "ARM A",
  comp_arm = c("ARM B", "ARM C")
)
#> adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>% 
#>     dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>% 
#>     dplyr::mutate(ARMCD = droplevels(ARMCD))

prepare_arm(
  dataname = "adsl",
  arm_var = "ARMCD",
  ref_arm = c("ARM B", "ARM C"),
  comp_arm = "ARM A"
)
#> adsl %>% dplyr::filter(ARMCD %in% c("ARM B", "ARM C", "ARM A")) %>% 
#>     dplyr::mutate(ARMCD = tern::combine_levels(ARMCD, levels = c("ARM B", 
#>         "ARM C"), new_level = "ARM B/ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
#>     ref = "ARM B/ARM C")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
```
