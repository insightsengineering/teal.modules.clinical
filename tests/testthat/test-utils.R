testthat::test_that("h_concat_expr returns a string for long expression", {
  expr <- quote(
    rtables::basic_table() %>%
      rtables::split_cols_by(var = "ARMCD") %>%
      tern::test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strata")
      ) %>%
      rtables::build_table(df = dta)
  )
  result <- h_concat_expr(expr)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("pipe_expr concatenate expressions into a single pipeline (%>%)", {
  result <- pipe_expr(
    list(
      expr1 = substitute(df),
      expr2 = substitute(head())
    )
  )
  expected <- quote(df %>% head())
  testthat::expect_identical(result, expected)
})

testthat::test_that("add_expr adds expressions to expression list", {
  lyt <- list()
  lyt <- add_expr(lyt, substitute(rtables::basic_table()))
  lyt <- add_expr(
    lyt, substitute(rtables::split_cols_by(var = arm), env = list(armcd = "ARMCD"))
  )
  lyt <- add_expr(
    lyt,
    substitute(
      tern::test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strata")
      )
    )
  )
  result <- lyt <- add_expr(lyt, substitute(rtables::build_table(df = dta)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("add_expr manages expression list which can be used by pipe_expr", {
  lyt <- list()
  lyt <- add_expr(lyt, substitute(rtables::basic_table()))
  lyt <- add_expr(
    lyt, substitute(rtables::split_cols_by(var = arm), env = list(armcd = "ARMCD"))
  )
  lyt <- add_expr(
    lyt,
    substitute(
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strata")
      )
    )
  )
  lyt <- add_expr(lyt, substitute(rtables::build_table(df = dta)))
  result <- pipe_expr(lyt)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

adrs <- tmc_ex_adrs
expr1 <- substitute(
  expr = anl <- subset(df, PARAMCD == param),
  env = list(df = as.name("adrs"), param = "INVET")
)
expr2 <- substitute(expr = anl$rsp_lab <- tern::d_onco_rsp_label(anl$AVALC))
expr3 <- substitute(
  expr = anl$is_rsp <- anl$rsp_lab %in%
    c("Complete Response (CR)", "Partial Response (PR)")
)

testthat::test_that("bracket_expr concatenates expressions into a single expression", {
  result <- bracket_expr(list(expr1, expr2, expr3))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("bracket_expr returns a single evaluable expression", {
  eval(bracket_expr(list(expr1, expr2, expr3)))
  result <- table(anl$rsp_lab, anl$is_rsp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# prepare_arm ----
testthat::test_that("prepare_arm with standard inputs", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prepare_arm combine ref arms", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = c("ARM C")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prepare_arm combine ref arms and use new level", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = c("ARM C"),
    ref_arm_val = "Control"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prepare_arm does not do anything when we don't want to compare or drop arms", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = NULL,
    comp_arm = c("ARM C"),
    drop = FALSE
  )

  expected <- quote(
    adrs
  )

  testthat::expect_equal(result, expected)

  result2 <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = "BLA",
    compare_arm = FALSE,
    comp_arm = c("ARM C"),
    drop = FALSE
  )
  testthat::expect_identical(result, result2)
})

# prepare_arm_levels ----
testthat::test_that("prepare_arm_levels with standard inputs", {
  result <- prepare_arm_levels(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ARMCD",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("prepare_arm_levels can use parentname arm levels", {
  result <- prepare_arm_levels(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ARMCD",
    drop_arm_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("color_lab_values main", {
  vals <- c("3 HIGH", "2 NORMAL", "5 HIGH", "4", "0 LOW")
  result <- color_lab_values(vals)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("color_lab_values neutral for none characters", {
  vals <- 1:5

  testthat::expect_identical(color_lab_values(vals), vals)
  testthat::expect_identical(color_lab_values(letters), letters)
})

testthat::test_that("clean_description", {
  vals <- rownames(mtcars)
  vals2 <- 1:10

  testthat::expect_identical(clean_description(vals), vals)
  testthat::expect_identical(clean_description(vals2), as.character(vals2))
})

testthat::test_that("get_g_forest_obj_var_name", {
  paramcd <- teal.transform::data_extract_spec(
    dataname = "ADSL",
    filter = teal.transform::filter_spec(
      vars = c("PARAMCD", "CNSR"),
      sep = "-",
      choices = c("os1" = "OS-1", "os0" = "OS-0", "pfs1" = "PFS-1"),
      selected = "OS-1",
      multiple = FALSE
    )
  )
  input <- list("paramcd-dataset_ADSL_singleextract-filter1-vals" = c("OS-1"))
  result <- get_g_forest_obj_var_name(paramcd, input)
  expected <- "os1"
  testthat::expect_equal(result, expected)
})

testthat::test_that("cs_to_select_spec single choices_selected is converted to select_spec", {
  testthat::expect_identical(
    cs_to_select_spec(teal.transform::choices_selected(letters, "a")),
    teal.transform::select_spec(letters, "a")
  )
})

testthat::test_that("cs_to_select_spec single choices_selected is converted to select_spec with multiple = TRUE", {
  testthat::expect_identical(
    cs_to_select_spec(teal.transform::choices_selected(letters, "a"), multiple = TRUE),
    teal.transform::select_spec(letters, "a", multiple = TRUE)
  )
})

testthat::test_that("cs_to_select_spec single choices_selected is converted to select_spec with ordered = TRUE", {
  testthat::expect_identical(
    cs_to_select_spec(teal.transform::choices_selected(letters, "a"), ordered = TRUE),
    teal.transform::select_spec(letters, "a", ordered = TRUE)
  )
})

testthat::test_that("cs_to_select_spec accepts choices_selected only", {
  testthat::expect_error(
    cs_to_select_spec(c("a", "b"))
  )
})

testthat::test_that("cs_to_select_spec accepts multiple as flag only", {
  testthat::expect_error(
    cs_to_select_spec(teal.transform::choices_selected(letters, "a"), multiple = 1L)
  )
})

testthat::test_that("cs_to_select_spec accepts ordered as flag only", {
  testthat::expect_error(
    cs_to_select_spec(teal.transform::choices_selected(letters, "a"), ordered = 1L)
  )
})

testthat::test_that("cs_to_filter_spec fails if doesn't have var_choices", {
  testthat::expect_error(
    cs_to_filter_spec(teal.transform::choices_selected(letters, "a"))
  )
})

testthat::test_that("cs_to_filter_spec creates filter_spec from choices_selected", {
  data <- data.frame(a = letters, b = LETTERS)
  testthat::expect_identical(
    cs_to_filter_spec(
      teal.transform::choices_selected(value_choices(data, "a"), "a")
    ),
    teal.transform::filter_spec(
      vars = "a",
      choices = teal.transform::choices_selected(value_choices(data, "a"), "a")$choices,
      selected = "a"
    )
  )
})

testthat::test_that("cs_to_des_select creates data_extract_spec with single select and no filter", {
  testthat::expect_identical(
    cs_to_des_select(
      teal.transform::choices_selected(letters, "a"),
      dataname = "test"
    ),
    teal.transform::data_extract_spec(
      dataname = "test",
      select = teal.transform::select_spec(choices = letters, selected = "a")
    )
  )
})

testthat::test_that("cs_to_des_select returns the input if data_extract_spec is provided", {
  testthat::expect_identical(
    cs_to_des_select(
      teal.transform::data_extract_spec(
        dataname = "test",
        select = teal.transform::select_spec(choices = letters, selected = "a")
      ),
      dataname = "test"
    ),
    teal.transform::data_extract_spec(
      dataname = "test",
      select = teal.transform::select_spec(choices = letters, selected = "a")
    )
  )
})

testthat::test_that("cs_to_des_select creates data_extract_spec with multiple = TRUE", {
  testthat::expect_identical(
    cs_to_des_select(teal.transform::choices_selected(letters, "a"), dataname = "test", multiple = TRUE),
    teal.transform::data_extract_spec(
      dataname = "test", select = teal.transform::select_spec(choices = letters, selected = "a", multiple = TRUE)
    )
  )
})

testthat::test_that("cs_to_des_select creates data_extract_spec with ordered = TRUE", {
  testthat::expect_identical(
    cs_to_des_select(teal.transform::choices_selected(letters, "a"), dataname = "test", ordered = TRUE),
    teal.transform::data_extract_spec(
      dataname = "test", select = teal.transform::select_spec(choices = letters, selected = "a", ordered = TRUE)
    )
  )
})

testthat::test_that("as_numeric_from_comma_sep_str returns NULL if blank string or NULL entered", {
  testthat::expect_null(as_numeric_from_comma_sep_str(NULL))
  testthat::expect_null(as_numeric_from_comma_sep_str("   "))
})

testthat::test_that("as_numeric_from_comma_sep_str returns numeric vector of numbers", {
  testthat::expect_equal(as_numeric_from_comma_sep_str("3,4,5.56"), c(3, 4, 5.56))
  testthat::expect_equal(as_numeric_from_comma_sep_str("3,4   ,v"), c(3, 4, NA))
})

testthat::test_that("as_numeric_from_comma_sep_str respects sep argument", {
  testthat::expect_equal(as_numeric_from_comma_sep_str("3,4,5", sep = ";"), as.numeric(NA))
  testthat::expect_equal(as_numeric_from_comma_sep_str("3 %% 4   %% 154.32", sep = "%%"), c(3, 4, 154.32))
})

testthat::test_that("default_total_label works properly", {
  testthat::expect_silent(set_default_total_label("Total Pts"))
  testthat::expect_equal(default_total_label(), "Total Pts")
  testthat::expect_silent(set_default_total_label("All Patients"))
})
