
# teal.modules.clinical

The `teal.modules.clinical` R package contains interactive `teal` modules for the outputs
(TLGs) in [`tern`](https://github.roche.com/NEST/tern).

See a live demo [on the Roche Shiny Server](http://shiny.roche.com/spada/teal_demo/).

To build your own app, see [Getting Started](#getting-started) below.

## Training

We are currently working on trainings. Here is a list of available content:


 * [Agile-R Training Website](http://go.roche.com/agile-R)
 * [go.gene.com/go-teal: a gentle introduction and getting started webpage](http://go.gene.com/go-teal)
 * [General Teal and Agile R Framework](http://pdwebdev01.gene.com/groups/devo/multimedia/Gen_Teal/story.html)

Videos explaining the indiviual teal modules can be found under [Articles](https://pages.github.roche.com/NEST/teal.tern/articles/) on the web documentation.

# Installation

## Stable Release

Please install the package dependencies as follows:

``` r
devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github("Roche/rtables", ref = "v0.1.0",
  upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "v0.0.4", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)
```

## Development Version

``` r
devtools::install_github(
  repo = "NEST/random.cdisc.data",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github("Roche/rtables", ref = "master",
  upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "NEST/teal",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "NEST/teal.tern",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)
```

# Getting Started

Here is an example app that shows all modules using random data. If you save
this code into a file named `app.R` then it is a valid [single-file shiny
application](https://shiny.rstudio.com/articles/app-formats.html).

## Simple app

Copy the following code into a new R file and execute it line by line:

```r
library(teal.modules.clinical)
library(random.cdisc.data)

ASL <- radsl(N = 600)

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
library(dplyr)

## Generate Data
ASL <- radsl(seed = 1) %>% 
  mutate(., 
         RACE = droplevels(RACE),
         ARM1 = sample(c("DUMMY A", "DUMMY B"), n(), TRUE),
         STRATM1 = sample(paste("STRATM1", 1:3), n(), TRUE),
         STRATM2 = sample(paste("STRATM2", 1:4), n(), TRUE),
         TCICLVL2 = sample(paste("STRATM2", letters[1:3]), n(), TRUE),
         T1I1FL = sample(c(T,F), n(), TRUE),
         T0I0FL = sample(c(T,F), n(), TRUE),
         ITTGEFL = sample(c(T,F), n(), TRUE),
         ITTWTFL = sample(c(T,F), n(), TRUE),
         ITTGE2FL = sample(c(T,F), n(), TRUE),
         ITTGE3FL = sample(c(T,F), n(), TRUE))

ATE <- radtte(ADSL = ASL, seed = 1)
ARS <- radrs(ADSL = ASL, seed = 1)

attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1) %>% mutate(...)"
attr(ATE, "source") <- "random.cdisc.data::radtte(ASL, seed = 1)"
attr(ARS, "source") <- "random.cdisc.data::radrs(ASL, seed = 1)"

## Reusable Configuration For Modules
arm_var_selected <- "ARM"
arm_var_choices <- c("ARM", "ARMCD", "ARM1", "STRATM1", "STRATM2")
arm_var <- choices_selected(arm_var_choices, arm_var_selected)

strata_var_selected <- c("STRATM1", "STRATM2", "TCICLVL2") %>%
  intersect(names(ASL))
strata_var_choices <-  names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))
strata_var <- choices_selected(strata_var_choices, strata_var_selected)

dm_summarize_vars_selected <- c("SEX", "BAGE", "RACE") %>% 
  intersect(names(ASL))
dm_summarize_vars_choices <- names(ASL)
dm_summarize_vars <- choices_selected(dm_summarize_vars_choices, dm_summarize_vars_selected)

facet_var_selected <- "TCICLVL2"
facet_var_choices <- names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))
facet_var <- choices_selected(facet_var_choices, facet_var_selected)

paramcd_tte_selected <- "OS"
paramcd_tte_choices <- unique(ATE$PARAMCD)
paramcd_tte <- choices_selected(paramcd_tte_choices, paramcd_tte_selected)

paramcd_rsp_selected <- "INVET"
paramcd_rsp_choices <- c("BESRSPI", "INVET", "OVRINV") %>% 
  intersect(ARS$PARAMCD)
paramcd_rsp <- choices_selected(paramcd_rsp_choices, paramcd_rsp_selected)

x_var_ct_selected <- "T1I1FL"
y_var_ct_selected <- "T0I0FL"
ct_choices <- c("T1I1FL", "T0I0FL", "ITTGEFL", "ITTWTFL", "ITTGE2FL", "ITTGE3FL") %>%
  intersect(names(ASL))
x_var_ct <- choices_selected(ct_choices, x_var_ct_selected)
y_var_ct <- choices_selected(ct_choices, y_var_ct_selected)

# reference & comparison arm selection when switching the arm variable
arm_ref_comp <- list(
  ACTARMCD = list(ref = "ARM A", comp = c("ARM B", "ARM C")),
  ARM1 = list(ref = "DUMMY B", comp = "DUMMY A")
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
      arm_var = arm_var,
      summarize_vars = dm_summarize_vars
    ),
    modules(
      "Forest Plots",
      tm_g_forest_tte(
        label = "Survival Forest Plot",
        dataname = "ATE",
        arm_var = arm_var,
        subgroup_var = strata_var,
        paramcd = paramcd_tte,
        plot_height = c(800, 200, 4000)
      ),
      tm_g_forest_rsp(
        label = "Response Forest Plot",
        dataname = "ARS",
        arm_var = arm_var,
        subgroup_var = strata_var,
        paramcd = paramcd_rsp,
        plot_height = c(800, 200, 4000)
      )
    ),
    tm_g_km(
      label = "Kaplan Meier Plot",
      dataname = "ATE",
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_tte,
      facet_var = facet_var,
      strata_var = strata_var,
      plot_height = c(1800, 200, 4000)
    ),
    tm_t_rsp(
      label = "Response Table",
      dataname = "ARS",
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_rsp,
      strata_var = strata_var
    ),
    # @TODO
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ATE",
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = paramcd_tte,
      strata_var = strata_var,
      time_points = choices_selected(c(6, 12, 18, 24, 30, 36, 42), c(6, 12, 18)),
      time_unit = "month",
      event_desrc_var = "EVNTDESC"
    ),
    tm_t_percentage_cross_table(
      "Cross Table",
      dataname = "ASL",
      x_var = x_var_ct,
      y_var = y_var_ct
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


Each teal module in `teal` will be explained in a separate vignette and
is accessile via the articles tab on the [project site][ghs].


## Deployment

See on the [agile-R website](teal/deployment/).


[ghs]: http://pages.github.roche.com/NEST/teal.tern
