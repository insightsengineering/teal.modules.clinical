# Observer for Treatment reference variable

Updates the reference and comparison Treatments when the selected
Treatment variable changes

## Usage

``` r
arm_ref_comp_observer(
  session,
  input,
  output,
  id_ref = "Ref",
  id_comp = "Comp",
  id_arm_var,
  data,
  arm_ref_comp,
  module,
  on_off = reactive(TRUE),
  input_id = "buckets",
  output_id = "arms_buckets"
)
```

## Arguments

- session:

  (`environment`)  
  shiny session

- input:

  (`character`)  
  shiny input

- output:

  (`character`)  
  shiny input

- id_ref:

  (`character`)  
  id of reference Treatment input UI element

- id_comp:

  (`character`)  
  id of comparison group input UI element

- id_arm_var:

  (`character`)  
  id of Treatment variable input UI element

- data:

  (`reactive` or `data.frame`)  
  dataset used to validate Treatment reference inputs and set `id_ref`
  input.

- arm_ref_comp:

  (`unknown`)  
  Treatment reference and compare variables provided as a nested list
  where each Treatment variable corresponds a list specifying the
  default levels for the reference and comparison treatments.

- module:

  (`character`)  
  name of the module where this is called (this is only used to produce
  more informative error messages)

- on_off:

  (`logical`)  
  A reactive that can be used to stop the whole observer if `FALSE`.

- input_id:

  (`character`)  
  unique id that the buckets will be referenced with.

- output_id:

  (`character`)  
  name of the UI id that the output will be written to.

## Value

Returns a
[`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html)
which checks that there is at least one reference and comparison arm
