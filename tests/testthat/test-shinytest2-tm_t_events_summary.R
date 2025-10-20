app_driver_tm_t_events_summary <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- teal.data::rADSL %>%
      mutate(
        DTHFL = case_when(
          !is.na(DTHDT) ~ "Y",
          TRUE ~ ""
        ) %>% with_label("Subject Death Flag")
      )
    ADAE <- teal.data::rADAE

    .add_event_flags <- function(dat) {
      dat <- dat %>%
        dplyr::mutate(
          TMPFL_SER = AESER == "Y",
          TMPFL_REL = AEREL == "Y",
          TMPFL_GR5 = AETOXGR == "5",
          TMP_SMQ01 = !is.na(SMQ01NAM),
          TMP_SMQ02 = !is.na(SMQ02NAM),
          TMP_CQ01 = !is.na(CQ01NAM)
        )
      column_labels <- list(
        TMPFL_SER = "Serious AE",
        TMPFL_REL = "Related AE",
        TMPFL_GR5 = "Grade 5 AE",
        TMP_SMQ01 = tern::aesi_label(dat[["SMQ01NAM"]], dat[["SMQ01SC"]]),
        TMP_SMQ02 = tern::aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
        TMP_CQ01 = tern::aesi_label(dat[["CQ01NAM"]])
      )
      col_labels(dat)[names(column_labels)] <- as.character(column_labels)
      dat
    }

    ADAE <- ADAE %>% .add_event_flags()

    .ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
    .aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_")]
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_events_summary(
        label = "Adverse Events Summary",
        dataname = "ADAE",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices("ADSL", c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        flag_var_anl = teal.transform::choices_selected(
          choices = teal.transform::variable_choices("ADAE", data[[".ae_anl_vars"]]),
          selected = data[[".ae_anl_vars"]][1],
          keep_order = TRUE,
          fixed = FALSE
        ),
        flag_var_aesi = teal.transform::choices_selected(
          choices = teal.transform::variable_choices("ADAE", data[[".aesi_vars"]]),
          selected = data[[".aesi_vars"]][1],
          keep_order = TRUE,
          fixed = FALSE
        ),
        dthfl_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], "DTHFL"),
          selected = "DTHFL", fixed = TRUE
        ),
        dcsreas_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], "DCSREAS"),
          selected = "DCSREAS", fixed = TRUE
        ),
        llt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AEDECOD"),
          selected = "AEDECOD", fixed = TRUE
        ),
        aeseq_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AESEQ"),
          selected = "AESEQ", fixed = TRUE
        ),
        add_total = TRUE,
        total_label = default_total_label(),
        na_level = default_na_str(),
        count_subj = TRUE,
        count_pt = TRUE,
        count_events = TRUE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_events_summary: Module initializes in teal without errors and produces table output.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_summary()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_summary: Starts with specified label, arm_var, flag_var_anl, flag_var_aesi,
  add_total, count_subj, count_pt, count_events.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_summary()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Adverse Events Summary"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("flag_var_anl-dataset_ADAE_singleextract-select"),
      "TMPFL_SER"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("flag_var_aesi-dataset_ADAE_singleextract-select"),
      "TMP_SMQ01"
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("count_subj"))
    testthat::expect_true(app_driver$get_active_module_input("count_pt"))
    testthat::expect_true(app_driver$get_active_module_input("count_events"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_events_summary: Selecting arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_summary()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARMCD")
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

testthat::test_that("e2e - tm_t_events_summary: Deselection of arm_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_summary()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "Please select exactly 1 or 2 treatment variables"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_summary: Selecting flag_var_anl changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_summary()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("flag_var_anl-dataset_ADAE_singleextract-select", c("TMPFL_REL", "TMPFL_GR5"))
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

testthat::test_that(
  "e2e - tm_t_events_summary: Deselection of flag_var_anl changes the table
  and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_summary()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("flag_var_anl-dataset_ADAE_singleextract-select", NULL)
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


testthat::test_that(
  "e2e - tm_t_events_summary: Selecting flag_var_aesi changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_summary()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("flag_var_aesi-dataset_ADAE_singleextract-select", c("TMP_SMQ02", "TMP_CQ01"))
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

testthat::test_that(
  "e2e - tm_t_events_summary: Deselection of flag_var_aesi changes the table
  and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_summary()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("flag_var_aesi-dataset_ADAE_singleextract-select", NULL)
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
