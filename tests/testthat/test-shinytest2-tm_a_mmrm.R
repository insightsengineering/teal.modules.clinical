app_driver_tm_a_mmrm <- function(fit_model = TRUE) {
  arm_ref_comp <- list(
    ARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs %>%
      dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
      dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
      dplyr::mutate(
        AVISIT = as.factor(AVISIT),
        AVISITN = rank(AVISITN) %>%
          as.factor() %>%
          as.numeric() %>%
          as.factor() #' making consecutive numeric factor
      )
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  arm_var <- choices_selected(c("ARM", "ARMCD"), "ARM")

  app_driver <- init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_a_mmrm(
        label = "MMRM",
        dataname = "ADQS",
        parentname = ifelse(inherits(arm_var, "data_extract_spec"),
          teal.transform::datanames_input(arm_var), "ADSL"
        ),
        aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
        id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
        arm_var = arm_var,
        visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
        arm_ref_comp = arm_ref_comp,
        paramcd = choices_selected(
          choices = value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
          selected = "FKSI-FWB"
        ),
        cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL),
        method = teal.transform::choices_selected(c(
          "Satterthwaite", "Kenward-Roger",
          "Kenward-Roger-Linear"
        ), "Satterthwaite", keep_order = TRUE),
        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
          keep_order =
            TRUE
        ),
        plot_height = c(700L, 200L, 2000L),
        plot_width = NULL,
        total_label = default_total_label(),
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args(),
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    ),
    timeout = 30000
  )
  if (fit_model) {
    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
  }
  app_driver
}

output_functions <- c(
  "t_mmrm_lsmeans",
  "g_mmrm_lsmeans",
  "t_mmrm_cov",
  "t_mmrm_fixed",
  "t_mmrm_diagnostic",
  "g_mmrm_diagnostic"
)

testthat::test_that(
  "e2e - tm_a_mmrm: Module initializes in teal without errors and displays a message to click 'Fit Model'.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_a_mmrm(FALSE)
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()

    null_text <- app_driver$namespaces(TRUE)$module("null_input_msg")

    testthat::expect_match(null_text, "Please first specify 'Model Settings' and press 'Fit Model'")

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_a_mmrm: Module initializes with specified label, aval_var, paramcd,
  visit_var, cov_var, arm_var, buckets, combine_comp_arms, id_var, cor_struct,
  weights_emmeans, conf_level, method, parallel and output_function.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm(FALSE)

    testthat::expect_equal(app_driver$get_text("#teal-teal_modules-active_tab .active"), "MMRM")

    testthat::expect_equal(app_driver$get_active_module_input("aval_var-dataset_ADQS_singleextract-select"), "AVAL")

    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals"),
      "FKSI-FWB"
    )

    testthat::expect_equal(app_driver$get_active_module_input("visit_var-dataset_ADQS_singleextract-select"), "AVISIT")

    testthat::expect_null(app_driver$get_active_module_input("cov_var-dataset_ADQS_singleextract-select"))

    testthat::expect_equal(app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"), "ARM")

    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("A: Drug X"),
        Comp = list("B: Placebo", "C: Combination")
      )
    )

    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))

    testthat::expect_equal(app_driver$get_active_module_input("id_var-dataset_ADQS_singleextract-select"), "USUBJID")

    testthat::expect_equal(app_driver$get_active_module_input("weights_emmeans"), "proportional")

    testthat::expect_equal(app_driver$get_active_module_input("cor_struct"), "unstructured")

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")

    testthat::expect_equal(app_driver$get_active_module_input("method"), "Satterthwaite")

    testthat::expect_true(app_driver$get_active_module_input("parallel"))

    testthat::expect_equal(app_driver$get_active_module_input("output_function"), "t_mmrm_lsmeans")

    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_a_mmrm: Click on fit model shows table for default selection.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_mmrm()
  app_driver$expect_no_validation_error()

  table <- app_driver$get_active_module_table_output("mmrm_table-table-with-settings")
  col_val <- app_driver$get_active_module_input("buckets")
  testthat::expect_true(all(unlist(col_val, use.names = FALSE) %in% colnames(table)))
  testthat::expect_equal(nrow(table), 25)

  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_a_mmrm: Function t_mmrm_lsmeans selection shows output settings; changing
  settings throws no validation errors and verify visibility of generated tables.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm()

    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("output_function", "t_mmrm_lsmeans", wait_ = FALSE)
    app_driver$expect_no_validation_error()

    testthat::expect_equal(app_driver$get_active_module_input("t_mmrm_lsmeans_show_relative"), "reduction")
    app_driver$set_active_module_input("t_mmrm_lsmeans_show_relative", "increase")
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_a_mmrm: Function g_mmrm_lsmeans selection shows output settings; changing
  settings throws no validation errors and verify visibility of generated plots.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm()

    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("output_function", "g_mmrm_lsmeans", wait_ = FALSE)
    app_driver$expect_no_validation_error()

    plot_before <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot_before, "data:image/png;base64,")

    app_driver$set_active_module_input("g_mmrm_lsmeans_select", "estimates")
    app_driver$expect_no_validation_error()
    app_driver$set_active_module_input("g_mmrm_lsmeans_select", "contrasts")
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input(
      "g_mmrm_lsmeans_select",
      c("estimates", "contrasts")
    )
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("g_mmrm_lsmeans_width", 0.9)
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("g_mmrm_lsmeans_contrasts_show_pval", TRUE)
    app_driver$expect_no_validation_error()

    plot <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot, "data:image/png;base64,")

    testthat::expect_false(identical(plot_before, plot))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_a_mmrm: Function g_mmrm_diagnostic selection shows output settings; changing
  settings throws no validation errors and verify visibility of generated plots.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm()

    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("output_function", "g_mmrm_diagnostic", wait_ = FALSE)
    app_driver$expect_no_validation_error()

    plot_before <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot_before, "data:image/png;base64,")

    app_driver$set_active_module_input("g_mmrm_diagnostic_type", "q-q-residual")
    app_driver$expect_no_validation_error()

    plot <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot, "data:image/png;base64,")

    testthat::expect_false(identical(plot_before, plot))
    app_driver$stop()
  }
)

for (func in output_functions) {
  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of aval_var throws validation error in method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      app_driver$set_active_module_input("aval_var-dataset_ADQS_singleextract-select", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$namespaces(TRUE)$module(
          sprintf(
            "%s .shiny-validation-message",
            ns_des_input("aval_var", "ADQS", "select_input")
          )
        ),
        "Analysis Variable' field is not selected"
      )
      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection paramcd throws validation error in method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      app_driver$set_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$namespaces(TRUE)$module(
          sprintf(
            "%s .shiny-validation-message",
            ns_des_input("paramcd", "ADQS", "filter1-vals_input")
          )
        ),
        "Select Endpoint' field is not selected"
      )
      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of visit_var throws validation error in method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      app_driver$set_active_module_input("visit_var-dataset_ADQS_singleextract-select", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$namespaces(TRUE)$module(
          sprintf(
            "%s .shiny-validation-message",
            ns_des_input("visit_var", "ADQS", "select_input")
          )
        ),
        "Visit Variable' field is not selected"
      )
      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of arm_var throws validation error in method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$namespaces(TRUE)$module(
          sprintf(
            "%s .shiny-validation-message",
            ns_des_input("arm_var", "ADSL", "select_input")
          )
        ),
        "Treatment variable must be selected"
      )
      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of id_var throws validation error in method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      app_driver$set_active_module_input("id_var-dataset_ADQS_singleextract-select", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$namespaces(TRUE)$module(
          sprintf(
            "%s .shiny-validation-message",
            ns_des_input("id_var", "ADQS", "select_input")
          )
        ),
        "Subject Identifier' field is not selected"
      )
      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of conf_level throws validation error in method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      app_driver$set_active_module_input("conf_level", numeric(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$namespaces(TRUE)$module(
          sprintf(
            "%s .shiny-validation-message",
            "conf_level_input"
          )
        ),
        "Confidence Level' field is not selected"
      )
      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )
}

input_list <- list(
  "aval_var-dataset_ADQS_singleextract-select" = "CHG",
  "paramcd-dataset_ADQS_singleextract-filter1-vals" = "BFIALL",
  "visit_var-dataset_ADQS_singleextract-select" = "AVISITN",
  "cov_var-dataset_ADQS_singleextract-select" = "AGE",
  "arm_var-dataset_ADSL_singleextract-select" = "ARMCD",
  "combine_comp_arms" = TRUE,
  "id_var-dataset_ADQS_singleextract-select" = "SUBJID",
  "weights_emmeans" = "equal",
  "cor_struct" = "ante-dependence",
  "conf_level" = "0.8",
  "method" = "Kenward-Roger"
)

non_responsive_conditions <- list(
  "g_mmrm_lsmeans" = c("id_var-dataset_ADQS_singleextract-select"),
  "g_mmrm_diagnostic" = c(
    "arm_var-dataset_ADSL_singleextract-select",
    "id_var-dataset_ADQS_singleextract-select",
    "weights_emmeans",
    "cor_struct",
    "conf_level",
    "method"
  )
)
# TODO: Remove the conditional skipping logic once the following issues are resolved:
# Issue 1153: https://github.com/insightsengineering/teal.modules.clinical/issues/1153
# Issue 1151: https://github.com/insightsengineering/teal.modules.clinical/issues/1151

# Iterate over each output function
for (func in output_functions) {
  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Validate output on different selection on method %s.",
      func
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()


      if (grepl("^g_", func)) {
        plot_before <- app_driver$get_active_module_plot_output("mmrm_plot")
      } else {
        table_before <- app_driver$get_active_module_table_output("mmrm_table-table-with-settings")
      }

      # Iterate over each input and test changes
      for (input_name in names(input_list)) {
        if (input_name %in% non_responsive_conditions[[func]]) {
          next
        }

        app_driver$set_active_module_input(input_name, input_list[[input_name]])
        app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
        app_driver$wait_for_idle()
        app_driver$expect_no_validation_error()

        # Check output based on function type (plot or table)
        if (grepl("^g_", func)) {
          testthat::expect_false(
            identical(
              plot_before,
              app_driver$get_active_module_plot_output("mmrm_plot")
            )
          )
          plot_before <- app_driver$get_active_module_plot_output("mmrm_plot")
        } else {
          testthat::expect_false(
            identical(
              table_before,
              app_driver$get_active_module_table_output("mmrm_table-table-with-settings")
            )
          )
        }
      }
      app_driver$stop()
    }
  )
}
