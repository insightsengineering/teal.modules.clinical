# Get full label, useful for annotating plots

Get full label, useful for annotating plots

## Usage

``` r
column_annotation_label(dataset, column, omit_raw_name = FALSE)
```

## Arguments

- dataset:

  (`data.frame`)  
  dataset

- column:

  (`character`)  
  column to get label from

- omit_raw_name:

  (`logical`)  
  omits the raw name in square brackets if label is found

## Value

"Label `[Column name]`" if label exists, otherwise "Column name".

## Examples

``` r
data <- mtcars
column_annotation_label(data, "cyl")
#> [1] "cyl"
attr(data[["cyl"]], "label") <- "Cylinder"
column_annotation_label(data, "cyl")
#> [1] "Cylinder [cyl]"
column_annotation_label(data, "cyl", omit_raw_name = TRUE)
#> [1] "Cylinder"
column_annotation_label(tmc_ex_adsl, "ACTARM")
#> [1] "Description of Actual Arm [ACTARM]"
```
