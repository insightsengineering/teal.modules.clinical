app_driver_tm_a_mmrm <- function(fit_model = TRUE) {
  arm_ref_comp <- list(ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")))

  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs |>
      dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") |>
      dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) |>
      dplyr::mutate(
        AVISIT = as.factor(AVISIT),
        AVISITN = rank(AVISITN) |>
          as.factor() |>
          as.numeric() |>
          as.factor() #' making consecutive numeric factor
      )
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  testthat::expect_warning(
    paramcd_values <- teal.picks::values(selected = "FKSI-FWB", multiple = FALSE),
    "doesn't guarantee that `selected` is a subset of `choices`.",
    fixed = TRUE
  )

  app_driver <- init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_a_mmrm(
        label = "MMRM",
        dataname = "ADQS",
        parentname = "ADSL",
        aval_var = teal.picks::variables(c("AVAL", "CHG")),
        id_var = teal.picks::variables(c("USUBJID", "SUBJID")),
        arm_var = teal.picks::variables(c("ARM", "ARMCD")),
        visit_var = teal.picks::variables(c("AVISIT", "AVISITN")),
        arm_ref_comp = arm_ref_comp,
        paramcd_values = paramcd_values,
        cov_var = teal.picks::variables(c("BASE", "AGE", "SEX", teal.picks::interaction_vars("BASE", "AVISIT")), NULL),
        method = teal.picks::values(c("Satterthwaite", "Kenward-Roger", "Kenward-Roger-Linear"), "Satterthwaite"),
        conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95"),
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
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_a_mmrm(FALSE)
    withr::defer(app_driver$stop())
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()

    null_text <- app_driver$get_text(app_driver$namespaces(TRUE)$module("null_input_msg"))

    testthat::expect_match(null_text, "Please first specify 'Model Settings' and press 'Fit Model'")
  }
)

testthat::test_that(
  "e2e - tm_a_mmrm: Module initializes with specified label, aval_var, paramcd,
  visit_var, cov_var, arm_var, buckets, combine_comp_arms, id_var, cor_struct,
  weights_emmeans, conf_level, method, parallel and output_function.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm(FALSE)
    withr::defer(app_driver$stop())

    testthat::expect_equal(app_driver$get_text("a.nav-link.active"), "MMRM")

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(exported_values[["aval_var-picks_resolved"]]$variables$selected, "AVAL")

    testthat::expect_equal(exported_values[["paramcd-picks_resolved"]]$values$selected, "FKSI-FWB")

    testthat::expect_equal(exported_values[["visit_var-picks_resolved"]]$variables$selected, "AVISIT")
    testthat::expect_null(exported_values[["cov_var-picks_resolved"]]$variables$selected)

    testthat::expect_equal(exported_values[["arm_var-picks_resolved"]]$variables$selected, "ARM")

    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(Ref = list("A: Drug X"), Comp = list("B: Placebo", "C: Combination"))
    )

    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))

    testthat::expect_equal(exported_values[["id_var-picks_resolved"]]$variables$selected, "USUBJID")

    testthat::expect_equal(app_driver$get_active_module_input("weights_emmeans"), "proportional")

    testthat::expect_equal(app_driver$get_active_module_input("cor_struct"), "unstructured")

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")

    testthat::expect_equal(app_driver$get_active_module_input("method"), "Satterthwaite")

    testthat::expect_true(app_driver$get_active_module_input("parallel"))

    testthat::expect_equal(app_driver$get_active_module_input("output_function"), "t_mmrm_lsmeans")
  }
)

testthat::test_that("e2e - tm_a_mmrm: Click on fit model shows table for default selection.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_mmrm()
  withr::defer(app_driver$stop())
  app_driver$expect_no_validation_error()

  table <- app_driver$get_active_module_table_output("mmrm_table-table-with-settings")
  col_val <- app_driver$get_active_module_input("buckets")
  testthat::expect_true(all(unlist(col_val, use.names = FALSE) %in% colnames(table)))
  testthat::expect_equal(nrow(table), 25)
})

testthat::test_that(
  "e2e - tm_a_mmrm: Function t_mmrm_lsmeans selection shows output settings; changing
  settings throws no validation errors and verify visibility of generated tables.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm()
    withr::defer(app_driver$stop())

    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("output_function", "t_mmrm_lsmeans", wait_ = FALSE)
    app_driver$expect_no_validation_error()

    testthat::expect_equal(app_driver$get_active_module_input("t_mmrm_lsmeans_show_relative"), "reduction")
    app_driver$set_active_module_input("t_mmrm_lsmeans_show_relative", "increase")
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_a_mmrm: Function g_mmrm_lsmeans selection shows output settings; changing
  settings throws no validation errors and verify visibility of generated plots.",
  {
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_a_mmrm()
    withr::defer(app_driver$stop())

    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("output_function", "g_mmrm_lsmeans", wait_ = FALSE)
    app_driver$expect_no_validation_error()
    app_driver$wait_for_idle()

    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("mmrm_plot-plot_main"))

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
    app_driver$wait_for_idle()

    plot <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot, "data:image/png;base64,")

    testthat::expect_false(identical(plot_before, plot))
  }
)

testthat::test_that(
  "e2e - tm_a_mmrm: Function g_mmrm_diagnostic selection shows output settings; changing
  settings throws no validation errors and verify visibility of generated plots.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_mmrm()
    withr::defer(app_driver$stop())

    app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
    app_driver$wait_for_idle()
    app_driver$expect_no_validation_error()

    app_driver$set_active_module_input("output_function", "g_mmrm_diagnostic", wait_ = FALSE)
    app_driver$expect_no_validation_error()
    app_driver$wait_for_idle()

    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("mmrm_plot-plot_main"))

    plot_before <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot_before, "data:image/png;base64,")

    app_driver$set_active_module_input("g_mmrm_diagnostic_type", "q-q-residual")
    app_driver$expect_no_validation_error()
    app_driver$wait_for_idle()

    plot <- app_driver$get_active_module_plot_output("mmrm_plot")
    testthat::expect_match(plot, "data:image/png;base64,")

    testthat::expect_false(identical(plot_before, plot))
  }
)

for (func in output_functions) {
  id_selector <- if (grepl("^g_", func)) {
    "mmrm_plot-plot_out_main"
  } else {
    "mmrm_table-table_out_main"
  }
  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of aval_var throws validation error in method %s.",
      func
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      set_teal_picks_slot(app_driver, "aval_var", "variables", character(0L))

      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$get_text(app_driver$namespaces(TRUE)$module(id_selector)),
        "A analysis variable must be selected.",
        fixed = TRUE
      )
      app_driver$expect_validation_error()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection paramcd throws validation error in method %s.",
      func
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$get_text(app_driver$namespaces(TRUE)$module(id_selector)),
        "A select endpoint must be selected."
      )
      app_driver$expect_validation_error()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of visit_var throws validation error in method %s.",
      func
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      set_teal_picks_slot(app_driver, "visit_var", "variables", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$get_text(app_driver$namespaces(TRUE)$module(id_selector)),
        "A visit variable must be selected."
      )
      app_driver$expect_validation_error()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of arm_var throws validation error in method %s.",
      func
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      set_teal_picks_slot(app_driver, "arm_var", "variables", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$get_text(app_driver$namespaces(TRUE)$module(id_selector)),
        "A treatment variable must be selected."
      )
      app_driver$expect_validation_error()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of id_var throws validation error in method %s.",
      func
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()

      set_teal_picks_slot(app_driver, "id_var", "variables", character(0L))
      if (grepl("^g_", func)) {
        testthat::expect_identical(app_driver$get_active_module_plot_output("mmrm_plot"), character(0))
      } else {
        testthat::expect_identical(
          app_driver$get_active_module_table_output("mmrm_table-table-with-settings"), data.frame()
        )
      }

      testthat::expect_match(
        app_driver$get_text(app_driver$namespaces(TRUE)$module(id_selector)),
        "A subject identifier must be selected."
      )
      app_driver$expect_validation_error()
    }
  )

  testthat::test_that(
    sprintf(
      "e2e - tm_a_mmrm: Deselection of conf_level throws validation error in method %s.",
      func
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
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
        app_driver$get_text(app_driver$namespaces(TRUE)$module(id_selector)),
        "A confidence level must be selected."
      )
      app_driver$expect_validation_error()
    }
  )
}

input_list <- list(
  "aval_var" = quote(set_teal_picks_slot(app_driver, "aval_var", "variables", "CHG")),
  "paramcd" = quote(set_teal_picks_slot(app_driver, "paramcd", "values", "BFIALL")),
  "visit_var" = quote(set_teal_picks_slot(app_driver, "visit_var", "variables", "AVISITN")),
  "cov_var" = quote(set_teal_picks_slot(app_driver, "cov_var", "variables", "AGE")),
  "arm_var" = quote(set_teal_picks_slot(app_driver, "arm_var", "variables", "ARMCD")),
  "combine_comp_arms" = TRUE,
  "id_var" = quote(set_teal_picks_slot(app_driver, "id_var", "variables", "SUBJID")),
  "weights_emmeans" = "equal",
  "cor_struct" = "ante-dependence",
  "conf_level" = "0.8",
  "method" = "Kenward-Roger"
)

non_responsive_conditions <- list(
  "g_mmrm_lsmeans" = c("id_var"),
  "g_mmrm_diagnostic" = c(
    "arm_var",
    "id_var",
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
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_a_mmrm()
      withr::defer(app_driver$stop())
      # Set initial output function
      app_driver$set_active_module_input("output_function", func, wait_ = FALSE)
      app_driver$expect_no_validation_error()
      app_driver$wait_for_idle()

      if (grepl("^g_", func)) {
        app_driver$expect_visible(app_driver$namespaces(TRUE)$module("mmrm_plot-plot_main"))

        plot_before <- app_driver$get_active_module_plot_output("mmrm_plot")
      } else {
        table_before <- app_driver$get_active_module_table_output("mmrm_table-table-with-settings")
      }

      # Iterate over each input and test changes
      for (input_name in names(input_list)) {
        if (input_name %in% non_responsive_conditions[[func]]) {
          next
        }

        if (is.call(input_list[[input_name]])) {
          eval(input_list[[input_name]])
        } else {
          app_driver$set_active_module_input(input_name, input_list[[input_name]])
        }
        app_driver$click(selector = app_driver$namespaces(TRUE)$module("button_start"))
        app_driver$wait_for_idle()
        app_driver$expect_no_validation_error()

        # Check output based on function type (plot or table)
        if (grepl("^g_", func)) {
          plot_after <- app_driver$get_active_module_plot_output("mmrm_plot")
          testthat::expect_false(
            identical(plot_before, plot_after)
          )
          plot_before <- plot_after
        } else {
          testthat::expect_false(
            identical(
              table_before,
              app_driver$get_active_module_table_output("mmrm_table-table-with-settings")
            )
          )
        }
      }
    }
  )
}
