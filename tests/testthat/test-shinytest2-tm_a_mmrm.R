app_driver_tm_a_mmrm <- function() {

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

  datanames <- c("ADSL", "ADQS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_var <- choices_selected(c("ARM", "ARMCD"), "ARM")

  init_teal_app_driver(
    data = data,
    modules = tm_a_mmrm(
      label = "MMRM",
      dataname = "ADQS",
      parentname = ifelse(inherits(arm_var, "data_extract_spec"),
                          teal.transform::datanames_input(arm_var), "ADSL"),
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
      method = teal.transform::choices_selected(c("Satterthwaite", "Kenward-Roger",
                                                  "Kenward-Roger-Linear"), "Satterthwaite", keep_order = TRUE),
      conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
                                                      TRUE),
      plot_height = c(700L, 200L, 2000L),
      plot_width = NULL,
      total_label = default_total_label(),
      pre_output = NULL,
      post_output = NULL,
      basic_table_args = teal.widgets::basic_table_args(),
      ggplot2_args = teal.widgets::ggplot2_args()
    )
  )
}

testthat::test_that("e2e - tm_a_mmrm: Module initializes in teal without errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_mmrm()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  null_text <- app_driver$active_module_element_text("null_input_msg")

  testthat::expect_match(null_text, "Please first specify 'Model Settings' and press 'Fit Model'")

  app_driver$stop()
})
