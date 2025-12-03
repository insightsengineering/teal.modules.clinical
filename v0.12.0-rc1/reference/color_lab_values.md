# Mapping function for Laboratory Table

Map value and level characters to values with with proper html tags,
colors and icons.

## Usage

``` r
color_lab_values(
  x,
  classes = c("HIGH", "NORMAL", "LOW"),
  colors = list(HIGH = "red", NORMAL = "grey", LOW = "blue"),
  default_color = "black",
  icons = list(HIGH = "glyphicon glyphicon-arrow-up", LOW =
    "glyphicon glyphicon-arrow-down")
)
```

## Arguments

- x:

  (`character`)  
  vector with elements under the format (`value level`).

- classes:

  (`character`)  
  classes vector.

- colors:

  (`list`)  
  color per class.

- default_color:

  (`character`)  
  default color.

- icons:

  (`list`)  
  certain icons per level.

## Value

a character vector where each element is a formatted HTML tag
corresponding to a value in `x`.

## Examples

``` r
color_lab_values(c("LOW", "LOW", "HIGH", "NORMAL", "HIGH"))
#>                                                                                           LOW 
#> "<span style='color:blue!important'>LOW<i class='glyphicon glyphicon-arrow-down'></i></span>" 
#>                                                                                           LOW 
#> "<span style='color:blue!important'>LOW<i class='glyphicon glyphicon-arrow-down'></i></span>" 
#>                                                                                          HIGH 
#>   "<span style='color:red!important'>HIGH<i class='glyphicon glyphicon-arrow-up'></i></span>" 
#>                                                                                        NORMAL 
#>                        "<span style='color:grey!important'>NORMAL<i class='NULL'></i></span>" 
#>                                                                                          HIGH 
#>   "<span style='color:red!important'>HIGH<i class='glyphicon glyphicon-arrow-up'></i></span>" 
```
