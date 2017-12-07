
[Project Website][ghs]

# teal.oncology

This R package contains teal modules for analysing oncology clinical trials 
data. The package is in its building phase and under active development. Hence
do not use these modules for production work.


# Installation

You need to install `teal.oncology` and all its dependencies as follows:

``` r
devtools::install_github("Roche/rtables", upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_git(
  url = "http://github.roche.com/Rpackages/random.cdisc.data.git",
  build_vignettes = FALSE,
  upgrade_dependencies = FALSE
)

devtools::install_git(
  url = "http://github.roche.com/Rpackages/tern.git",
  build_vignettes = FALSE,
  upgrade_dependencies = FALSE
)

devtools::install_git(
  url = "http://github.roche.com/Rpackages/teal.git",
  build_vignettes = FALSE,
  upgrade_dependencies = FALSE,
  branch = "beta"
)

devtools::install_git(
  url = "http://github.roche.com/Rpackages/teal.oncology.git",
  build_vignettes = FALSE,
  upgrade_dependencies = FALSE,
  branch = "tern"
)
```

# Getting Started

Here is an example app that shows all modules using random data. If you save
this code into a file named `app.R` then it is a valid [single-file shiny
application](https://shiny.rstudio.com/articles/app-formats.html).

```r
library(teal.oncology)
library(random.cdisc.data)
library(dplyr)

## Generate Data
ASL <- radam("ASL", N = 600,
             arm_choices = c("Arm A", "Arm B", "Arm C"),
             start_with = list(
               STRATM1 = paste("STRATM1", 1:3),
               STRATM2 = paste("STRATM2", 1:4),
               TCICLVL2 = paste("STRATM2", letters[1:3]),
               T1I1FL = c(T,F),
               T0I0FL = c(T,F),
               ITTGEFL = c(T,F),
               ITTWTFL = c(T,F),
               ITTGE2FL = c(T,F),
               ITTGE3FL = c(T,F)
             )) %>%
  mutate(ARM1 = recode(ARM, "Arm A" = "DUMMY 1", "Arm B" = "DUMMY 2", "Arm C" = "DUMMY 3"))
ATE <- merge(ASL, radam("ATE", ADSL = ASL), all = TRUE)
ARS <- merge(ASL, radam("ARS", ADSL = ASL))
AQS <- radam("AQS", ADSL = ASL)

## Configure Reused Information
arm_var <- "ARM1"
arm_var_choices <- names(ASL)[(sapply(ASL, is.character))]  %>%
  intersect(names(ASL))

strata_var <- c("STRATM1", "STRATM2", "TCICLVL2") %>%
  intersect(names(ASL))
strata_var_choices <-  names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))

dm_summarize_vars <- c("SEX", "BAGE", "RACE") %>% 
  intersect(names(ASL))
dm_summarize_vars_choices <- names(ASL)

facet_var <-  "TCICLVL2"
facet_var_choices <- names(ASL)[(sapply(ASL, is.character))] %>%
  intersect(names(ASL))

paramcd_tte <- "OS"
paramcd_choices_tte <- unique(ATE$PARAMCD)

paramcd_rsp <- "OVRSPI"
paramcd_choices_rsp <- c("BESRSPI", "OVRINV", "OVRSPI") %>% 
  intersect(ARS$PARAMCD)

tmp.p.aqs <- AQS %>% select(PARAM, PARAMCD) %>% distinct() %>% mutate_all(as.character)
paramcd_aqs_choices <- setNames(tmp.p.aqs$PARAMCD, paste(tmp.p.aqs$PARAMCD, tmp.p.aqs$PARAM, sep = " - "))
paramcd_aqs <- paramcd_aqs_choices[1]

x_var_ct <- "T1I1FL"
y_var_ct <- "T0I0FL"
ct_var_choices <- c("T1I1FL", "T0I0FL", "ITTGEFL", "ITTWTFL", "ITTGE2FL", "ITTGE3FL") %>%
  intersect(names(ASL))

# unfortunatly this is only implemented for the tte_tbl currently
ref_arm <- sort(unique(ASL$ARM1))[1]

## Setup App
x <- teal::init(
  data = list(ASL = ASL, ARS = ARS, ATE = ATE, AQS = AQS),
  modules = root_modules(
    module(
      label = "Study Information",
      server = function(input, output, session, datasets) {},
      ui = function(id) {tags$p("Info about data source")},
      filters = NULL
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_demographic_table(
      label = "Demographic Table",
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      summarize_vars =  dm_summarize_vars,
      summarize_vars_choices = dm_summarize_vars_choices
    ),
    modules(
      "Forest Plots",
      tm_forest_survival(
        label = "Survival Forest Plot",
        paramcd = paramcd_tte,
        paramcd_choices = paramcd_choices_tte,
        subgroup_var = strata_var,
        subgroup_var_choices = strata_var_choices,
        arm_var = arm_var,
        arm_var_choices = arm_var_choices,
        plot_height = c(800, 200, 4000),
        cex = 1.2
      ),
      tm_forest_response(
        label = "Response Forest Plot",
        paramcd = paramcd_rsp,
        paramcd_choices = paramcd_choices_rsp,
        plot_height = c(800, 200, 4000),
        subgroup_var = strata_var,
        subgroup_var_choices = strata_var_choices,
        arm_var = arm_var,
        arm_var_choices = arm_var_choices,
        cex = 1.2
      )
    ),
    tm_kmplot(
      label = "Kaplan Meier Plot",
      treatment_var = arm_var,
      treatment_var_choices = arm_var_choices,
      endpoint = paramcd_tte,
      endpoint_choices = paramcd_choices_tte,
      facet_var = facet_var,
      facet_var_choices = facet_var_choices,
      strata_var = strata_var,
      strata_var_choices = strata_var_choices,
      plot_height = c(1800, 200, 4000)
    ), 
    tm_response_table(
      label = "Response Table",
      paramcd = "OVRSPI",
      paramcd_choices = paramcd_choices_rsp,
      arm.var = arm_var,
      arm.var_choices = arm_var_choices,
      strata.var = strata_var,
      strata.var_choices = strata_var_choices
    ),
    tm_time_to_event_table(
      label = "Time To Event Table",
      time_points = c(6, 12, 18),
      time_points_choices = c(6, 12, 18, 24, 30, 36, 42),
      time_points_unit = "months",
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      ref_arm = ref_arm,
      paramcd = paramcd_tte,
      paramcd_choices = paramcd_choices_tte,
      strata_var = strata_var,
      strata_var_choices = strata_var_choices,
      pre_output = shiny::helpText("Note that the 'p-value (log-rank)' is currently not matching that of STREAM")
    ),
    tm_percentage_cross_table(
      "Cross Table",
      x_var = x_var_ct,
      y_var = y_var_ct,
      x_var_choices = ct_var_choices,
      y_var_choices = ct_var_choices
    )
  ),
  modules(
    "QoL Change from Baseline",
    tm_chgfbl_plot(
      label = "Change from Baseline Plot",
      paramcd = paramcd_aqs,
      paramcd_choices = paramcd_aqs_choices,
      arm_var = arm_var,
      arm_var_choices = arm_var_choices,
      ytype = "CHG",
      ytype_choices = c("CHG", "AVAL"),
      errbar = "SE",
      errbar_choices = c("SE", "SD", "95CI", "IQR")
    ),
    tm_chgfbl_table(
      label = "Change from Baseline Table",
      paramcd = paramcd_aqs,
      paramcd_choices = paramcd_aqs_choices,
      arm_var = arm_var,
      arm_var_choices = arm_var_choices
    )
  ),
  header = div(
    class="",
    style="margin-bottom: 2px;",
    tags$h1("Example App With teal.oncology Teal Modules", tags$span("SPA", class="pull-right"))
  ),
  footer = tags$p(class="text-muted", "Info About Authors")
)  

shinyApp(x$ui, x$server)
```


Each teal module in `teal.oncology` will be explained in a separate vignette and
is accessile via the articles tab on the [project site][ghs].


[ghs]: http://pages.github.roche.com/Rpackages/teal.oncology