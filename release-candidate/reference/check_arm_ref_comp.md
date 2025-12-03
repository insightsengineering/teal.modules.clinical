# Check if the Treatment variable is reference or compare

Check Treatment variable type.

## Usage

``` r
check_arm_ref_comp(x, df_to_check, module)
```

## Arguments

- x:

  (`character`)  
  Name of the variable

- df_to_check:

  (`data.frame`)  
  table to check

- module:

  (`character`)  
  teal module the ref and comp are called in

## Value

`TRUE` or `FALSE` whether the variable is in ref or comp
