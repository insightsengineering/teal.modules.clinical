# We take the example from https://pages.github.roche.com/NEST/docs/hugo/NEST/agile-R/devel/teal/teal_modules/clinical-code/
# Before, we must have installed everything

library(teal.modules.clinical)
library(random.cdisc.data)

## Generate Data
ADSL <- radsl(seed = 1)
ADTTE <- radtte(ADSL = ADSL, seed = 1)
ADRS <- subset(radrs(ADSL = ADSL, seed = 1), AVISIT == "Follow Up")

attr(ADSL, "source") <- "random.cdisc.data::radsl(seed = 1)"
attr(ADTTE, "source") <- "random.cdisc.data::radtte(ADSL, seed = 1)"
attr(ADRS, "source") <- 'subset(random.cdisc.data::radrs(ADSL, seed = 1), AVISIT == "Follow Up")'

## Reusable Configuration For Modules
cs_arm_var <- choices_selected(c("ARM", "ARMCD", "ARM1", "STRATA1", "STRATA2"), "ARM")

cs_strata_var <- choices_selected(c("STRATA1", "STRATA2"), "STRATA1")

cs_facet_var <- choices_selected(c("STRATA1", "STRATA2", "SEX"), "STRATA1")

cs_paramcd_tte <- choices_selected(ADTTE$PARAMCD, "OS")

cs_paramcd_rsp <- choices_selected(ADRS$PARAMCD)

fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))


# reference & comparison arm selection when switching the arm variable
arm_ref_comp <- list(
  ARMCD = list(ref = "ARM A", comp = c("ARM B", "ARM C")),
  ARM = list(ref = "A: Drug X", comp = "B: Placebo")
)

## Setup App
app <- teal::init(
  data = cdisc_dataset(cdisc_data("ADSL", ADSL), cdisc_dataset("ADRS", ADRS), cdisc_dataset("ADTTE", ADTTE), code = 'ADSL <- radsl(seed = 1)
ADTTE <- radtte(ADSL = ADSL, seed = 1)
ADRS <- subset(radrs(ADSL = ADSL, seed = 1), AVISIT == "Follow Up")'),
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
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = cs_arm_var,
      summarize_vars = choices_selected(
        choices = setdiff(names(ADSL), c("USUBJID", "SUBJID")),
        selected = c("SEX", "AGE", "RACE")
      )
    ),
    modules(
      "Forest Plots",
      tm_g_forest_tte(
        label = "Survival Forest Plot",
        dataname = "ADTTE",
        arm_var = cs_arm_var,
        subgroup_var = cs_strata_var,
        paramcd = cs_paramcd_tte
      ),
      tm_g_forest_rsp(
        label = "Response Forest Plot",
        dataname = "ADRS",
        arm_var = cs_arm_var,
        subgroup_var = cs_strata_var,
        paramcd = cs_paramcd_rsp
      )
    ),
    tm_g_km(
      label = "Kaplan Meier Plot",
      dataname = "ADTTE",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_tte,
      facet_var = cs_facet_var,
      strata_var = cs_strata_var
    ),
    tm_t_rsp(
      label = "Response Table",
      dataname = "ADRS",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_rsp,
      strata_var = cs_strata_var
    ),
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ADTTE",
      arm_var = cs_arm_var,
      paramcd = cs_paramcd_tte,
      strata_var = cs_strata_var,
      time_points = choices_selected(c(6, 12, 18, 24, 30, 36, 42), c(6, 12, 18)),
      time_unit = "month"
    )
  ),
  header = div(
    class = "",
    style = "margin-bottom: 2px;",
    tags$h1("Example App with teal.modules.clinical modules", tags$span("SPA", class="pull-right"))
  ),
  footer = tags$p(class="text-muted", "Info About Authors")
)

shinyApp(app$ui, app$server)
