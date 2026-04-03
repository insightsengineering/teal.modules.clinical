# Manual playground for teal.picks badge-dropdown + bootstrap-select behaviour.
#
# Mirrors wiring used on picks-migration modules (e.g. tm_t_coxreg): nested
# `picks_ui` / `picks_srv` with `id = ""` inside `moduleServer`, so encodings
# sit under `{teal_module_ns}-paramcd-…` and `{teal_module_ns}-arm_var-…`.
#
# Run from the package root (so devtools finds DESCRIPTION), after optional
# `devtools::load_all()` on sibling repos if you need dev versions:
#
#   setwd("C:/Rprojects/teal.modules.clinical")
#   devtools::load_all(".")
#   source("dev/app_teal_picks_shinytest2_playground.R")
#
# Notes for shinytest2 / Chromote helpers (see tests/testthat/helper-TealAppDriver.R):
# - Picker inputs are not Shiny-bound until each badge panel has been opened
#   at least once (teal.picks badge-dropdown script runs Shiny.bindAll on show).
# - Prefer reading the native `<select id="…-values-selected">` (or
#   `…-variables-selected`) after open+close, and driving changes via DOM +
#   selectpicker + Shiny.setInputValue + pulsing `…-selected_open`, not CSS
#   clicks on ids with special characters.
#
# Automated checks: tests/testthat/test-shinytest2-teal_picks_playground.R

devtools::load_all(".")

paramcd_pick <- teal.picks::picks(
  teal.picks::datasets("ADTTE", "ADTTE"),
  teal.picks::variables(choices = "PARAMCD"),
  teal.picks::values()
)

arm_pick <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(
    choices = c("ARM", "ARMCD", "ACTARMCD"),
    selected = "ARM"
  )
)

picks_playground_ui <- function(id, paramcd, arm_var) {
  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::tagList(
      shiny::tags$p(
        shiny::tags$strong(
          "Open each encoding badge once before expecting picker inputs to bind."
        )
      ),
      shiny::tags$h4("Resolved picks (reactiveVal from picks_srv)"),
      shiny::verbatimTextOutput(ns("resolved")),
      shiny::tags$hr(),
      shiny::tags$h4("Raw Shiny inputs (after binding)"),
      shiny::verbatimTextOutput(ns("raw_inputs"))
    ),
    encoding = shiny::tagList(
      shiny::tags$label("Encodings", class = "text-primary"),
      shiny::tags$br(),
      shiny::tags$div(
        shiny::tags$label("Endpoint (datasets + variables + values)"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      shiny::tags$div(
        shiny::tags$label("Treatment (datasets + variables)"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      )
    )
  )
}

picks_playground_server <- function(id, data, paramcd, arm_var) {
  shiny::moduleServer(id, function(input, output, session) {
    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(paramcd = paramcd, arm_var = arm_var),
      data = data
    )

    output$resolved <- shiny::renderPrint({
      str(list(paramcd = selectors$paramcd(), arm_var = selectors$arm_var()))
    })

    output$raw_inputs <- shiny::renderPrint({
      # Subset of inputs that picks injects (ids depend on teal module ns).
      nms <- grep(
        "(paramcd|arm_var).*(selected|selected_open|datasets|variables)",
        names(input),
        value = TRUE
      )
      if (length(nms)) {
        str(stats::setNames(lapply(nms, function(nm) input[[nm]]), nms))
      } else {
        message("No matching inputs yet (badges not opened / not bound).")
      }
    })
  })
}

picks_playground_module <- teal::module(
  label = "teal.picks playground",
  server = picks_playground_server,
  ui = picks_playground_ui,
  ui_args = list(paramcd = paramcd_pick, arm_var = arm_pick),
  server_args = list(paramcd = paramcd_pick, arm_var = arm_pick),
  datanames = c("ADSL", "ADTTE")
)

data <- teal.data::teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
})
teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

app <- teal::init(
  data = data,
  modules = teal::modules(picks_playground_module)
)

shiny::shinyApp(app$ui, app$server)
