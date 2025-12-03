# Extracts html id for `data_extract_ui`

The `data_extract_ui` is located under extended html id. We could not
use `ns("original id")` for reference, as it is extended with specific
suffixes.

## Usage

``` r
extract_input(varname, dataname, filter = FALSE)
```

## Arguments

- varname:

  (`character`)  
  the original html id. This should be retrieved with
  `ns("original id")` in the UI function or
  `session$ns("original id")`/"original id" in the server function.

- dataname:

  (`character`)  
  `dataname` from data_extract input. This might be retrieved like
  `data_extract_spec(...)[[1]]$dataname`.

- filter:

  (`logical`) optional,  
  if the connected `extract_data_spec` has objects passed to its
  `filter` argument

## Value

a string

## Examples

``` r
extract_input("ARM", "ADSL")
#> [1] "ARM-dataset_ADSL_singleextract-select"
```
