# Decorate Module Output

## Introduction

The outputs produced by `teal` modules, like graphs or tables, are
created by the module developer and look a certain way. It is hard to
design an output that will satisfy every possible user, so the form of
the output should be considered a default value that can be customized.
Here we describe the concept of *decoration*, enabling the app developer
to tailor outputs to their specific requirements without rewriting the
original module code.

The decoration process is build upon transformation procedures,
introduced in `teal`. While `transformators` are meant to edit module’s
input, decorators are meant to adjust the module’s output. To
distinguish the difference, modules in `teal.modules.clinical` have 2
separate parameters: `transformators` and `decorators`.

To get a complete understanding refer the following vignettes:

- Transforming the input data in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).
- Transforming module output in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html).

## Outputs that can be decorated

It is important to note which output objects from a given module can be
decorated. The module function documentation’s *Decorating Module*
section has this information.

You can also refer the table shown below to know which module outputs
can be decorated.

| Module | Outputs (Class) |
|----|----|
| `tm_a_gee` | table (ElementaryTable) |
| `tm_a_mmrm` | lsmeans_table (TableTree), lsmeans_plot (ggplot), covariance_table (ElementaryTable), fixed_effects_table (ElementaryTable), diagnostic_table (ElementaryTable), diagnostic_plot (ggplot) |
| `tm_g_barchart_simple` | plot (ggplot) |
| `tm_g_ci` | plot (ggplot) |
| `tm_g_forest_rsp` | plot (ggplot) |
| `tm_g_forest_tte` | plot (ggplot) |
| `tm_g_ipp` | plot (ggplot) |
| `tm_g_km` | plot (ggplot) |
| `tm_g_lineplot` | plot (ggplot) |
| `tm_g_pp_adverse_events` | table (datatables), plot (ggplot) |
| `tm_g_pp_patient_timeline` | plot (ggplot) |
| `tm_g_pp_therapy` | plot (ggplot), table (datatables) |
| `tm_g_pp_vitals` | plot (ggplot) |
| `tm_t_abnormality` | table (TableTree) |
| `tm_t_abnormality_by_worst_grade` | table (TableTree) |
| `tm_t_ancova` | table (TableTree) |
| `tm_t_binary_outcome` | table (TableTree) |
| `tm_t_coxreg` | table (TableTree) |
| `tm_t_events` | table (TableTree) |
| `tm_t_events_by_grade` | table (TableTree) |
| `tm_t_events_patyear` | table (ElementaryTable) |
| `tm_t_events_summary` | table (TableTree) |
| `tm_t_exposure` | table (ElementaryTable) |
| `tm_t_logistic` | table (TableTree) |
| `tm_t_mult_events` | table (TableTree) |
| `tm_t_pp_basic_info` | table (datatables) |
| `tm_t_pp_laboratory` | table (datatables) |
| `tm_t_pp_medical_history` | table (TableTree) |
| `tm_t_pp_prior_medication` | table (datatables) |
| `tm_t_shift_by_arm` | table (TableTree) |
| `tm_t_shift_by_arm_by_worst` | table (TableTree) |
| `tm_t_shift_by_grade` | table (TableTree) |
| `tm_t_smq` | table (TableTree) |
| `tm_t_summary` | table (TableTree) |
| `tm_t_summary_by` | table (TableTree) |
| `tm_t_tte` | table (TableTree) |

Also, note that there are three different types of objects that can be
decorated:

1.  `listing_df`, `ElementaryTable`, `TableTree`
2.  `ggplot`
3.  `datatables`

*Tip:* A general tip before trying to decorate the output from the
module is to copy the reproducible code and running them in a separate R
session to quickly iterate the decoration you want.

## Decorating `listing_df`, `ElementaryTable`, `TableTree`

Here’s an example to showcase how you can edit an output of class
`listing_df`, `ElementaryTable`, or `TableTree`. All these classes are
extension of objects created using `rtables` and can be modified with
the help of `rtables` modifiers like
[`rtables::insert_rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/insert_rrow.html).

``` r

library(teal.modules.clinical)

data <- within(teal_data(), {
  library(dplyr)
  ADSL <- tmc_ex_adsl |>
    mutate(
      ITTFL = factor("Y") |> with_label("Intent-To-Treat Population Flag")
    ) |>
    mutate(DTHFL = case_when(!is.na(DTHDT) ~ "Y", TRUE ~ "") |> with_label("Subject Death Flag"))


  ADLB <- tmc_ex_adlb |>
    mutate(AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min)) |>
    mutate(
      ONTRTFL = case_when(
        AVISIT %in% c("SCREENING", "BASELINE") ~ "",
        TRUE ~ "Y"
      ) |> with_label("On Treatment Record Flag")
    )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

insert_rrow_decorator <- function(default_caption = "I am a good new row") {
  teal_transform_module(
    label = "New row",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "new_row"), "New row", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                table <- rtables::insert_rrow(table, rtables::rrow(new_row))
              },
              new_row = input$new_row
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_t_abnormality(
      label = "tm_t_abnormality",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices("ADSL", subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      add_total = FALSE,
      by_vars = choices_selected(
        choices = variable_choices("ADLB", subset = c("LBCAT", "PARAM", "AVISIT")),
        selected = c("LBCAT", "PARAM"),
        keep_order = TRUE
      ),
      baseline_var = choices_selected(
        variable_choices("ADLB", subset = "BNRIND"),
        selected = "BNRIND", fixed = TRUE
      ),
      grade = choices_selected(
        choices = variable_choices("ADLB", subset = "ANRIND"),
        selected = "ANRIND",
        fixed = TRUE
      ),
      abnormal = list(low = "LOW", high = "HIGH"),
      exclude_base_abn = FALSE,
      decorators = list(table = insert_rrow_decorator("I am a good new row"))
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

## Decorating `ggplot`

Here’s an example to showcase how you can edit an output of class
`ggplot`. You can extend them using `ggplot2` functions.

``` r

library(teal.modules.clinical)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  require(nestcolor)
  ADSL <- rADSL
  ADTTE <- tmc_ex_adtte
})
join_keys(data) <- default_cdisc_join_keys[names(data)]


ggplot_caption_decorator <- function(default_caption = "I am a good decorator") {
  teal_transform_module(
    label = "Caption",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "title"), "Plot Title", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                plot <- plot +
                  ggplot2::ggtitle(title) +
                  cowplot::theme_cowplot()
              },
              title = input$title
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_g_km(
      label = "tm_g_km",
      dataname = "ADTTE",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      paramcd = choices_selected(
        value_choices("ADTTE", "PARAMCD", "PARAM"),
        "OS"
      ),
      arm_ref_comp = list(
        ACTARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
        ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
      ),
      strata_var = choices_selected(
        variable_choices("ADSL", c("SEX", "BMRKR2")),
        "SEX"
      ),
      facet_var = choices_selected(
        variable_choices("ADSL", c("SEX", "BMRKR2")),
        NULL
      ),
      decorators = list(plot = ggplot_caption_decorator())
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```

## Decorating `datatables`

Here’s an example to showcase how you can edit an output of class
`datatables`. Please refer the [helper
functions](https://rstudio.github.io/DT/functions.html) of the `DT`
package to learn more about extending the `datatables` objects.

``` r

library(teal.modules.clinical)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  ADSL <- rADSL
  ADLB <- tmc_ex_adlb |>
    mutate(AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min)) |>
    mutate(
      ONTRTFL = case_when(
        AVISIT %in% c("SCREENING", "BASELINE") ~ "",
        TRUE ~ "Y"
      ) |> with_label("On Treatment Record Flag")
    )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

dt_table_decorator <- function(color1 = "pink", color2 = "lightblue") {
  teal_transform_module(
    label = "Table color",
    ui = function(id) {
      selectInput(
        NS(id, "color"),
        "Table Color",
        choices = c("white", color1, color2),
        selected = "Default"
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |> within(
            {
              table <- DT::formatStyle(
                table,
                columns = attr(table$x, "colnames")[-1],
                target = "row",
                backgroundColor = color
              )
            },
            color = input$color
          )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_t_pp_laboratory(
      label = "tm_t_pp_laboratory",
      dataname = "ADLB",
      patient_col = "USUBJID",
      paramcd = choices_selected(
        choices = variable_choices("ADLB", "PARAMCD"),
        selected = "PARAMCD"
      ),
      param = choices_selected(
        choices = variable_choices("ADLB", "PARAM"),
        selected = "PARAM"
      ),
      timepoints = choices_selected(
        choices = variable_choices("ADLB", "ADY"),
        selected = "ADY"
      ),
      anrind = choices_selected(
        choices = variable_choices("ADLB", "ANRIND"),
        selected = "ANRIND"
      ),
      aval_var = choices_selected(
        choices = variable_choices("ADLB", "AVAL"),
        selected = "AVAL"
      ),
      avalu_var = choices_selected(
        choices = variable_choices("ADLB", "AVALU"),
        selected = "AVALU"
      ),
      decorators = list(table = dt_table_decorator())
    )
  )
)
```

    ## Warning: The `decorators` argument of `tm_t_pp_laboratory()` is deprecated as of
    ## teal.modules.clinical 0.11.0.
    ## ℹ Decorators functionality was removed from this module. The `decorators`
    ##   argument will be ignored.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
