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


# TODO: This will be moved to the `TealAppDriver` class.
active_module_tws_output <- function(app_driver, element = "table-table-with-settings", which = 1) {
  app_driver$active_module_element(element) %>%
    app_driver$get_html_rvest() %>%
    rvest::html_table(fill = TRUE) %>%
    {
      if (identical(., list())) {
        .
      } else {
        .[[which]]
      }
    }
}
