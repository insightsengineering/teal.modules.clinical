# Initialization function to create a new TealAppDriver object
#
# By manipulating the server function as below, we can hint {shinytest2} to load
# this package and its "Depends".
# Related to https://github.com/rstudio/shinytest2/issues/381
init_teal_app_driver <- function(...) {
  testthat::with_mocked_bindings(
    {
      TealAppDriver <- getFromNamespace("TealAppDriver", "teal") # nolint: object_name.
      TealAppDriver$new(...)
    },
    shinyApp = function(ui, server, ...) {
      functionBody(server) <- bquote({
        # Hint to shinytest2 that this package should be available (via {globals})
        .hint_to_load_package <- tm_g_ci # Hint to shinytest2 when looking for packages in globals
        .(functionBody(server))
      })

      shiny::shinyApp(ui, server, ...)
    },
    # The relevant shinyApp call in `TealAppDriver` is being called without prefix,
    # hence why the package bindings that is changed is in {teal} and not {shiny}
    .package = "teal"
  )
}

ns_dataset <- function(prefix, suffix, dataset, extract = "singleextract") {
  sprintf("%s-dataset_%s_%s-%s", prefix, dataset, extract, suffix)
}

test_plot_changes_no_errors <- function(app_driver, input, value, pws) {
  plot_before <- app_driver$get_active_module_pws_output(pws)
  app_driver$set_active_module_input(input, value)
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output(pws)))
}

test_table_changes_no_errors <- function(app_driver, input, value, tws) {
  table_before <- app_driver$get_active_module_tws_output(tws)
  app_driver$set_active_module_input(input, value)
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(table_before, app_driver$get_active_module_tws_output(tws)))
}

test_validation_error <- function(app_driver, input, value = character(0), table = FALSE, message = NULL) {
  app_driver$set_active_module_input(input, value)

  if (table) {
    testthat::expect_identical(app_driver$get_active_module_tws_output("table"), data.frame())
  }

  app_driver$expect_validation_error()

  if (!is.null(message)) {
    testthat::expect_match(
      app_driver$active_module_element_text(sprintf("%s .shiny-validation-message", input)),
      message
    )
  }
}

test_no_validation_error <- function(app_driver, input_id, value) {
  app_driver$set_active_module_input(input_id, value)
  app_driver$expect_no_validation_error()
}
