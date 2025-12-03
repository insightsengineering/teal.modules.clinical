# Getting Started with {teal.modules.clinical}

### Introduction

`teal.modules.clinical` is a package implementing a number of `teal`
modules helpful for exploring clinical trials data, specifically
targeted towards data following the
[ADaM](https://www.cdisc.org/standards/foundational/adam) standards.
`teal.modules.clinical` modules can be used with data other than ADaM
standard clinical data, but some features of the package are tailored
towards data of this type.

The concepts presented here require knowledge about the core features of
`teal`, specifically on how to launch a `teal` application and how to
pass data into it. Therefore, it is highly recommended to refer to the
[home page](https://insightsengineering.github.io/teal/latest-tag/) and
[introductory
vignette](https://insightsengineering.github.io/teal/latest-tag/articles/getting-started-with-teal.html)
of the `teal` package.

### Main Features

The package provides ready-to-use `teal` modules you can embed in your
`teal` application. The modules generate highly customizable tables,
plots, and outputs often used in exploratory data analysis, including:

- ANCOVA -
  [`tm_t_ancova()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_ancova.md)
- Cox regression -
  [`tm_t_coxreg()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_coxreg.md)
- Kaplan-Meier plot -
  [`tm_g_km()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_km.md)
- Logistic regression -
  [`tm_t_logistic()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_logistic.md)
- Bar chart -
  [`tm_g_barchart_simple()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_barchart_simple.md)
- Confidence interval plot -
  [`tm_g_ci()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_ci.md)
- Binary outcome response table -
  [`tm_t_binary_outcome()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_binary_outcome.md)
- Summary of adverse events table -
  [`tm_t_events_summary()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_events_summary.md)
- SMQ table -
  [`tm_t_smq()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_smq.md)
- Time-to-event table -
  [`tm_t_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_tte.md)

The library also offers a group of patient profile modules targeted for
clinical statisticians and physicians who want to review data on a per
patient basis. The modules present data about patient’s adverse events,
their severity, the current therapy, their laboratory results and more.

See the full index of package functions & modules
[here](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/reference/index.html).

### A Simple Application

A `teal.modules.clinical` module needs to be embedded inside a
`shiny`/`teal` application to interact with it. A simple application
including a bar chart module could look like this:

``` r
library(teal.modules.clinical)
library(nestcolor)

ADSL <- tmc_ex_adsl
ADAE <- tmc_ex_adae

app <- init(
  data = cdisc_data(
    ADSL = ADSL,
    ADAE = ADAE,
    code = "
      ADSL <- tmc_ex_adsl
      ADAE <- tmc_ex_adae
    "
  ),
  modules = list(
    tm_g_barchart_simple(
      label = "ADAE Analysis",
      x = data_extract_spec(
        dataname = "ADAE",
        select = select_spec(
          choices = variable_choices(
            ADAE,
            c(
              "ARM", "ACTARM", "SEX",
              "RACE", "SAFFL", "STRATA2"
            )
          ),
          selected = "ACTARM",
          multiple = FALSE
        )
      )
    )
  )
)

shinyApp(app$ui, app$server)
```

#### Try it out in Shinylive

[Open in
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA6EOkxYcIY0sWpFGiiEoCCAEQDKAGQAEAHgC0l0jAIB9OAA9nUfqOpHjhgFEbe0cXd09+KDglJTRUYMtaaVJ2JUtLSNIoSwBeSwJ+WlEXTKhUiHT0kwtcy2rzXDTKk0DaloDGisrifjhahTAmyrqzKzsHJ1cPLx8h5v8g8dCpiKi5gaa5TvS+IRFRWrpRFLnHZwBzZ3oWAgALFlJnUVoYVBFy4fTqKHo4an6wO06tBqKxnqIBttPm5aqUpqRmARHqJUHACB9PulStB4AD2pC5sNRH80aRasSREinqj0YTPnciDIxLUAG4sWg-ETOBlM0QYzHDdpQgXdfkiyoDQxYACykMskoAwgAVKWyvDysCmAIADQJXXFErAWEMCoCcoGpkMADEreZzZqlcaVQAmDb68X6A1bOlEklIuD8PHK1V6g0wYSkWhvPp5K2Gcxan2WT2fFPpFP6fRKUS3JKsQzodhxAAkglouEsJeJjBZcD0YAAvgBdIA)

Consider consulting the documentation and examples of each module
(e.g. [`?tm_g_barchart_simple`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_barchart_simple.md)).
In many, you can also find useful links to the [TLG
Catalog](https://insightsengineering.github.io/tlg-catalog/stable/)
where additional example apps can be found.

`teal.modules.clinical` exports modules and needs support from other
libraries to run a `teal` app and flesh out its functionality. In the
example above,
[`tm_g_barchart_simple()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_barchart_simple.md)
is the only function from `teal.modules.clinical` whereas
[`init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)
is a `teal` function, `data_extract_spec()`, `select_spec()`, and
`variable_choices()` are `teal.transform` functions, and
[`cdisc_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/cdisc_data.html)
is a `teal.data` function.

Let’s break the above app down into pieces:

``` r
library(teal.modules.clinical)
library(nestcolor)
```

The above lines load the libraries used in this example. We will use the
example data provided in the `teal.modules.clinical` package:

``` r
ADSL <- tmc_ex_adsl
ADAE <- tmc_ex_adae
```

`nestcolor` is an optional package that can be loaded in to apply the
standardized NEST color palette to all module plots.

There is no need to load `teal` as `teal.modules.clinical` already
depends on it.

In the next step, we use `teal` to create `shiny` UI and server
functions that we can launch using `shiny`. The `data` argument tells
`teal` about the input data - the ADaM datasets `ADSL` and `ADAE` - and
the `modules` argument indicates the modules included in the
application. Here, we include only one module:
[`tm_g_barchart_simple()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_barchart_simple.md).

``` r
app <- init(
  data = cdisc_data(
    ADSL = ADSL,
    ADAE = ADAE,
    code = "
      ADSL <- tmc_ex_adsl
      ADAE <- tmc_ex_adae
    "
  ),
  modules = list(
    tm_g_barchart_simple(
      label = "ADAE Analysis",
      x = data_extract_spec(
        dataname = "ADAE",
        select = select_spec(
          choices = variable_choices(
            ADAE,
            c(
              "ARM", "ACTARM", "SEX",
              "RACE", "SAFFL", "STRATA2"
            )
          ),
          selected = "ACTARM",
          multiple = FALSE
        )
      )
    )
  )
)
```

Finally, we use `shiny` to launch the application:

``` r
shinyApp(app$ui, app$server)
```

Some `teal.modules.clinical` modules allow for the specification of
arguments using
[`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html),
such as the
[`tm_t_summary()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_summary.md)
module in the following example.

``` r
ADSL <- tmc_ex_adsl

app <- init(
  data = cdisc_data(ADSL = ADSL, code = "ADSL <- tmc_ex_adsl"),
  modules = list(
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(choices = c("ARM", "ARMCD"), selected = "ARM"),
      summarize_vars = choices_selected(
        choices = c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
        selected = c("SEX", "RACE")
      )
    )
  )
)

shinyApp(app$ui, app$server)
```

Please refer to the [API
reference](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/reference/)
of specific modules for more examples and information on the
customization options available.
