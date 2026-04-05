app_driver_tm_t_glm_counts <- function() { # nolint: object_length.
  data <- within(teal_data(), {
    library("tern")
    ADSL <- tern_ex_adsl
    ADTTE <- tern_ex_adtte
  })

  join_keys(data) <- default_cdisc_join_keys[names(data)]

  arm_ref_comp <- list(
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    ),
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    )
  )

  ADSL <- data[["ADSL"]]
  ADTTE <- data[["ADTTE"]]
  # Initialize the teal app
  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_glm_counts(
        dataname = "ADTTE",
        arm_var = teal.picks::variables(
          choices = c("ARM", "ARMCD", "ACTARMCD"),
          selected = "ARMCD"
        ),
        arm_ref_comp = arm_ref_comp,
        aval_var = teal.picks::variables(choices = "AVAL"),
        strata_var = teal.picks::variables(choices = "SEX", selected = NULL),
        offset_var = teal.picks::variables(choices = "AGE", selected = NULL),
        cov_var = teal.picks::variables(choices = "SITEID", selected = NULL)
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_t_glm_counts: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_glm_counts()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
    app_driver$stop()
  }
)


testthat::test_that(
  "e2e - tm_t_glm_counts: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_glm_counts()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-variables-selected", "ACTARMCD")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)
