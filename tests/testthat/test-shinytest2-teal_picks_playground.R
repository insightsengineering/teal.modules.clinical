# Companion to dev/app_teal_picks_shinytest2_playground.R: minimal teal app with two
# teal.picks encodings (paramcd + arm_var) to exercise helper-TealAppDriver.R picks
# helpers without running a full clinical module.

app_driver_teal_picks_playground <- function() { # nolint: object_length_linter.
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
        shiny::verbatimTextOutput(ns("resolved"))
      ),
      encoding = shiny::tagList(
        teal.picks::picks_ui(ns("paramcd"), paramcd),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
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

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = teal::modules(picks_playground_module)
    )
  )
}

testthat::test_that("e2e - teal picks playground: app starts without Shiny errors.", {
  skip_if_too_deep(5)
  app <- app_driver_teal_picks_playground()
  app$expect_no_shiny_error()
  app$stop()
})

testthat::test_that("e2e - teal picks playground: get_teal_picks_slot reads datasets / variables.", {
  skip_if_too_deep(5)
  app <- app_driver_teal_picks_playground()
  app$expect_no_shiny_error()
  testthat::expect_identical(get_teal_picks_slot(app, "paramcd", "datasets"), "ADTTE")
  testthat::expect_identical(get_teal_picks_slot(app, "paramcd", "variables"), "PARAMCD")
  testthat::expect_identical(get_teal_picks_slot(app, "arm_var", "datasets"), "ADSL")
  testthat::expect_identical(get_teal_picks_slot(app, "arm_var", "variables"), "ARM")
  app$stop()
})

testthat::test_that("e2e - teal picks playground: set_teal_picks_slot updates arm_var variables.", {
  skip_if_too_deep(5)
  app <- app_driver_teal_picks_playground()
  app$expect_no_shiny_error()
  set_teal_picks_slot(app, "arm_var", "variables", "ARMCD")
  testthat::expect_identical(get_teal_picks_slot(app, "arm_var", "variables"), "ARMCD")
  app$stop()
})
