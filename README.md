
# teal.modules.clinical

The `teal.modules.clinical` R package contains interactive `teal` modules for the outputs
(TLGs) in [`tern`](https://github.roche.com/NEST/tern).

Please follow the installation instructions and training on the [agile-R website](http://go.roche.com/agile-R).


## Simple app

Copy the following code into a new R file and execute it line by line:

```r
library(teal.modules.clinical)
library(random.cdisc.data)

ASL <- radsl(N = 600)
attr(ASL, "source") <- "random.cdisc.data::radsl(N = 600)"


x <- teal::init(
  data = list(ASL = ASL),
  modules = root_modules(
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ASL",
      arm_var = choices_selected(c("ACTARMCD", "ARMCD"), "ACTARMCD"),
      summarize_vars = choices_selected(c("BMRKR2", "SEX", "RACE"), c("BMRKR2", "SEX"))
    )
  )
)

shinyApp(x$ui, x$server)
```

This should start the teal web app. Now save the above code into an R file named
`app.R` and replace `radam` with `read_bce` and select a dataset from a study of
your choice. Start the app again and configure the arguments of
`tm_t_summary` according to the information you would like to
summarize in your `ASL` dataset.


## App setup with all available modules

```r
library(teal.modules.clinical)
library(random.cdisc.data)

## Generate Data
ASL <- radsl(seed = 1)
ATE <- radtte(ADSL = ASL, seed = 1)
ARS <- radrs(ADSL = ASL, seed = 1)

attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1)"
attr(ATE, "source") <- "random.cdisc.data::radtte(ASL, seed = 1)"
attr(ARS, "source") <- "random.cdisc.data::radrs(ASL, seed = 1)"

## Reusable Configuration For Modules
cs_arm_var <- choices_selected(c("ARM", "ARMCD", "ARM1", "STRATA1", "STRATA2"), "ARM")

cs_strata_var <- choices_selected(c("STRATA1", "STRATA2"), "STRATA1")

cs_facet_var <- choices_selected(c("STRATA1", "STRATA2", "SEX"), "STRATA1")

cs_paramcd_tte <- choices_selected(ATE$PARAMCD, "OS")

cs_paramcd_rsp <- choices_selected(ARS$PARAMCD)

fact_vars_asl <- names(Filter(isTRUE, sapply(ASL, is.factor)))


# reference & comparison arm selection when switching the arm variable
arm_ref_comp <- list(
  ARMCD = list(ref = "ARM A", comp = c("ARM B", "ARM C")),
  ARM = list(ref = "A: Drug X", comp = "B: Placebo")
)

## Setup App
x <- teal::init(
  data = list(ASL = ASL, ARS = ARS, ATE = ATE),
  modules = root_modules(
    module(
      label = "Study Information",
      server = function(input, output, session, datasets) {},
      ui = function(id) {
        tagList(
          tags$p("Info about data source:"),
          tags$p("Radom data is used that has been created with the ",
                 tags$code("random.cdisc.data"), "R package.")
        )
      },
      filters = NULL
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ASL",
      arm_var = cs_arm_var,
      summarize_vars = choices_selected(
        choices = setdiff(names(ASL), c("USUBJID", "SUBJID")), 
        selected = c("SEX", "AGE", "RACE")
      )
    ),
    modules(
      "Forest Plots",
      tm_g_forest_tte(
        label = "Survival Forest Plot",
        dataname = "ATE",
        arm_var = cs_arm_var,
        subgroup_var = cs_strata_var,
        paramcd = cs_paramcd_tte,
        plot_height = c(800, 200, 4000)
      ),
      tm_g_forest_rsp(
        label = "Response Forest Plot",
        dataname = "ARS",
        arm_var = cs_arm_var,
        subgroup_var = cs_strata_var,
        paramcd = cs_paramcd_rsp,
        plot_height = c(800, 200, 4000)
      )
    ),
    tm_g_km(
      label = "Kaplan Meier Plot",
      dataname = "ATE",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_tte,
      facet_var = cs_facet_var,
      strata_var = cs_strata_var,
      plot_height = c(1800, 200, 4000)
    ),
    tm_t_rsp(
      label = "Response Table",
      dataname = "ARS",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_rsp,
      strata_var = cs_strata_var
    ),
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ATE",
      arm_var = cs_arm_var,
      paramcd = cs_paramcd_tte,
      strata_var = cs_strata_var,
      time_points = choices_selected(c(6, 12, 18, 24, 30, 36, 42), c(6, 12, 18)),
      time_unit = "month",
      event_desrc_var = "EVNTDESC"
    ),
    tm_t_percentage_cross_table(
      "Cross Table",
      dataname = "ASL",
      x_var = choices_selected(fact_vars_asl, fact_vars_asl[1]),
      y_var = choices_selected(fact_vars_asl, fact_vars_asl[4])
    )
  ),
  header = div(
    class = "",
    style = "margin-bottom: 2px;",
    tags$h1("Example App With teal.modules.clinical module", tags$span("SPA", class="pull-right"))
  ),
  footer = tags$p(class="text-muted", "Info About Authors")
)  

shinyApp(x$ui, x$server)
```

