library(dplyr)
library(scda)
library(tern)

testthat::test_that("h_concat_expr returns a string for long expression", {
  expr <- quote(
    rtables::basic_table() %>%
      rtables::split_cols_by(var = "ARMCD") %>%
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      ) %>%
      rtables::build_table(df = dta)
  )
  result <- h_concat_expr(expr)
  expected <- paste0(
    "rtables::basic_table() %>% rtables::split_cols_by(var = \"ARMCD\") %>%      ",
    "test_proportion_diff(vars = \"rsp\", method = \"cmh\", ",
    "variables = list(strata = \"strat\")) %>%      rtables::build_table(df = dta)"
  )
  testthat::expect_identical(result, expected)
})


testthat::test_that("pipe_expr concatenate expressions into a single pipeline (%>%)", {
  result <- pipe_expr(
    list(
      expr1 = substitute(df),
      expr2 = substitute(head)
    )
  )
  expected <- quote(df %>% head) # styler: off
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
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      )
    )
  )

  result <- lyt <- add_expr(lyt, substitute(rtables::build_table(df = dta)))
  expected <- list(
    substitute(rtables::basic_table()),
    substitute(rtables::split_cols_by(var = arm)),
    substitute(test_proportion_diff(
      vars = "rsp", method = "cmh", variables = list(strata = "strat")
    )),
    substitute(rtables::build_table(df = dta))
  )
  testthat::expect_identical(result, expected)
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
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      )
    )
  )

  lyt <- add_expr(lyt, substitute(rtables::build_table(df = dta)))
  result <- pipe_expr(lyt)
  expected <- substitute(
    rtables::basic_table() %>%
      rtables::split_cols_by(var = arm) %>%
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      ) %>%
      rtables::build_table(df = dta)
  )
  testthat::expect_identical(result, expected)
})

adrs <- synthetic_cdisc_data("rcd_2021_05_05")$adrs
expr1 <- substitute(
  expr = anl <- subset(df, PARAMCD == param),
  env = list(df = as.name("adrs"), param = "INVET")
)
expr2 <- substitute(expr = anl$rsp_lab <- d_onco_rsp_label(anl$AVALC))
expr3 <- substitute(
  expr = anl$is_rsp <- anl$rsp_lab %in%
    c("Complete Response (CR)", "Partial Response (PR)")
)

testthat::test_that("bracket_expr concatenates expressions into a single expression", {
  result <- bracket_expr(list(expr1, expr2, expr3))
  expected <- substitute(
    expr = {
      anl <- subset(adrs, PARAMCD == "INVET")
      anl$rsp_lab <- d_onco_rsp_label(anl$AVALC)
      anl$is_rsp <- anl$rsp_lab %in% c(
        "Complete Response (CR)", "Partial Response (PR)"
      )
    }
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("bracket_expr returns a single evaluable expression", {
  eval(bracket_expr(list(expr1, expr2, expr3)))
  result <- table(anl$rsp_lab, anl$is_rsp)
  expected <- structure(
    c(0L, 18L, 0L, 73L, 44L, 164L, 0L, 101L, 0L, 0L),
    .Dim = c(5L, 2L),
    .Dimnames = structure(
      list(
        c(
          "Complete Response (CR)", "Not Evaluable (NE)",
          "Partial Response (PR)", "Progressive Disease (PD)",
          "Stable Disease (SD)"
        ),
        c("FALSE", "TRUE")
      ),
      .Names = c("", "")
    ),
    class = "table"
  )
  testthat::expect_identical(result, expected)
})


# prepare_arm ----
testthat::test_that("prepare_arm with standard inputs", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C")
  )

  expected <- quote(
    adrs %>%
      dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
      dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
      dplyr::mutate(ARMCD = droplevels(ARMCD))
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("prepare_arm combine ref arms", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = c("ARM C")
  )

  expected <- quote(
    adrs %>%
      dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
      dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
      dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>%
      dplyr::mutate(ARMCD = droplevels(ARMCD))
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("prepare_arm combine ref arms and use new level", {
  result <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = c("ARM C"),
    ref_arm_val = "Control"
  )

  expected <- quote(
    adrs %>%
      dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
      dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "Control")) %>%
      dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "Control")) %>%
      dplyr::mutate(ARMCD = droplevels(ARMCD))
  )

  testthat::expect_equal(result, expected)
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

  expected <- quote({
    adae <- adae %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
    arm_levels <- levels(adae[["ARMCD"]])
    adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
    adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
  })

  testthat::expect_equal(result, expected)
})

testthat::test_that("prepare_arm_levels can use parentname arm levels", {
  result <- prepare_arm_levels(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ARMCD",
    drop_arm_levels = FALSE
  )

  expected <- quote({
    adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
    arm_levels <- levels(adsl[["ARMCD"]])
    adae <- adae %>% dplyr::mutate(ARMCD = factor(ARMCD, levels = arm_levels))
  })

  testthat::expect_equal(result, expected)
})

testthat::test_that("color_lab_values main", {
  vals <- c("3 HIGH", "2 NORMAL", "5 HIGH", "4", "0 LOW")

  testthat::expect_identical(
    color_lab_values(vals),
    c(
      `3 HIGH` = "<span style='color:red!important'>3<i class='glyphicon glyphicon-arrow-up'></i></span>",
      `2 NORMAL` = "<span style='color:grey!important'>2<i class='NULL'></i></span>",
      `5 HIGH` = "<span style='color:red!important'>5<i class='glyphicon glyphicon-arrow-up'></i></span>",
      `4` = "4",
      `0 LOW` = "<span style='color:blue!important'>0<i class='glyphicon glyphicon-arrow-down'></i></span>"
    )
  )
})

testthat::test_that("color_lab_values neutral for none characters", {
  vals <- 1:5

  testthat::expect_identical(color_lab_values(vals), vals)

  testthat::expect_identical(color_lab_values(letters), letters)
})

testthat::test_that("clean_description", {
  vals <- rownames(mtcars)

  testthat::expect_identical(clean_description(vals), vals)

  vals2 <- 1:10

  testthat::expect_identical(clean_description(vals2), as.character(vals2))
})

testthat::test_that("get_g_forest_obj_var_name", {
  paramcd <- data_extract_spec(
    dataname = "ADSL",
    filter = filter_spec(
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
    cs_to_select_spec(teal::choices_selected(letters, "a")),
    teal::select_spec(letters, "a")
  )
})

testthat::test_that("cs_to_select_spec single choices_selected is converted to select_spec with multiple = TRUE", {
  testthat::expect_identical(
    cs_to_select_spec(teal::choices_selected(letters, "a"), multiple = TRUE),
    teal::select_spec(letters, "a", multiple = TRUE)
  )
})

testthat::test_that("cs_to_select_spec single choices_selected is converted to select_spec with ordered = TRUE", {
  testthat::expect_identical(
    cs_to_select_spec(teal::choices_selected(letters, "a"), ordered = TRUE),
    teal::select_spec(letters, "a", ordered = TRUE)
  )
})

testthat::test_that("cs_to_select_spec accepts choices_selected only", {
  testthat::expect_error(
    cs_to_select_spec(c("a", "b"))
  )
})

testthat::test_that("cs_to_select_spec accepts multiple as flag only", {
  testthat::expect_error(
    cs_to_select_spec(teal::choices_selected(letters, "a"), multiple = 1L)
  )
})

testthat::test_that("cs_to_select_spec accepts ordered as flag only", {
  testthat::expect_error(
    cs_to_select_spec(teal::choices_selected(letters, "a"), ordered = 1L)
  )
})

testthat::test_that("cs_to_filter_spec fails if doesn't have var_choices", {
  testthat::expect_error(
    cs_to_filter_spec(teal::choices_selected(letters, "a"))
  )
})

testthat::test_that("cs_to_des_select fails if choices_selected is not a choices_labeled", {
  data <- data.frame(a = letters, b = LETTERS)
  testthat::expect_identical(
    cs_to_filter_spec(
      teal::choices_selected(value_choices(data, "a"), "a")
    ),
    teal::filter_spec(
      vars = "a",
      choices = teal::choices_selected(value_choices(data, "a"), "a")$choices,
      selected = "a"
    )
  )
})

testthat::test_that("cs_to_des_select creates data_extract_spec with single select and no filter", {
  testthat::expect_identical(
    cs_to_des_select(
      teal::choices_selected(letters, "a"),
      dataname = "test"
    ),
    teal::data_extract_spec(
      dataname = "test",
      select = select_spec(choices = letters, selected = "a")
    )
  )
})

testthat::test_that("cs_to_des_select returns the input if data_extract_spec is provided", {
  testthat::expect_identical(
    cs_to_des_select(
      teal::data_extract_spec(
        dataname = "test",
        select = select_spec(choices = letters, selected = "a")
      ),
      dataname = "test"
    ),
    teal::data_extract_spec(
      dataname = "test",
      select = select_spec(choices = letters, selected = "a")
    )
  )
})

testthat::test_that("cs_to_des_select creates data_extract_spec with multiple = TRUE", {
  testthat::expect_identical(
    cs_to_des_select(teal::choices_selected(letters, "a"), dataname = "test", multiple = TRUE),
    teal::data_extract_spec(dataname = "test", select = select_spec(choices = letters, selected = "a", multiple = TRUE))
  )
})

testthat::test_that("cs_to_des_select creates data_extract_spec with ordered = TRUE", {
  testthat::expect_identical(
    cs_to_des_select(teal::choices_selected(letters, "a"), dataname = "test", ordered = TRUE),
    teal::data_extract_spec(dataname = "test", select = select_spec(choices = letters, selected = "a", ordered = TRUE))
  )
})
