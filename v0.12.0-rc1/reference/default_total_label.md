# Default string for total column label

The default string used as a label for the "total" column. This value is
used as the default value for the `total_label` argument throughout the
`teal.modules.clinical` package. If not specified for each module by the
user via the `total_label` argument, or in the R environment options via
`set_default_total_label()`, then `"All Patients"` is used.

## Usage

``` r
default_total_label()

set_default_total_label(total_label)
```

## Arguments

- total_label:

  (`string`)  
  Single string value to set in the R environment options as the default
  label to use for the "total" column. Use
  `getOption("tmc_default_total_label")` to check the current value set
  in the R environment (defaults to `"All Patients"` if not set).

## Value

- `default_total_label` returns the current value if an R environment
  option has been set for `"tmc_default_total_label"`, or
  `"All Patients"` otherwise.

&nbsp;

- `set_default_total_label` has no return value.

## Functions

- `default_total_label()`: Getter for default total column label.

- `set_default_total_label()`: Setter for default total column label.
  Sets the option `"tmc_default_total_label"` within the R environment.

## Examples

``` r
# Default settings
default_total_label()
#> [1] "All Patients"
getOption("tmc_default_total_label")
#> NULL

# Set custom value
set_default_total_label("All Patients")

# Settings after value has been set
default_total_label()
#> [1] "All Patients"
getOption("tmc_default_total_label")
#> [1] "All Patients"
```
