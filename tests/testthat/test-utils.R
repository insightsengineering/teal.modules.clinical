library(dplyr)
library(scda)
library(tern)

test_that("h_concat_expr returns a string for long expression", {
  expr <- quote(
    basic_table() %>%
      split_cols_by(var = "ARMCD") %>%
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      ) %>%
      build_table(df = dta)
  )
  result <- h_concat_expr(expr)
  expected <- paste0(
    "basic_table() %>% split_cols_by(var = \"ARMCD\") %>% ",
    "test_proportion_diff(vars = \"rsp\",      method = \"cmh\", ",
    "variables = list(strata = \"strat\")) %>% build_table(df = dta)"
  )
  expect_identical(result, expected)
})


test_that("pipe_expr concatenate expressions into a single pipeline (%>%)", {
  result <- pipe_expr(
    list(
      expr1 = substitute(df),
      expr2 = substitute(head)
    )
  )
  expected <- quote(df %>% head)
  expect_identical(result, expected)
})

test_that("add_expr adds expressions to expression list", {
  lyt <- list()
  lyt <- add_expr(lyt, substitute(basic_table()))
  lyt <- add_expr(
    lyt, substitute(split_cols_by(var = arm), env = list(armcd = "ARMCD"))
  )
  lyt <- add_expr(
    lyt,
    substitute(
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      )
    )
  )

  result <- lyt <- add_expr(lyt, substitute(build_table(df = dta)))
  expected <- list(
    substitute(basic_table()),
    substitute(split_cols_by(var = arm)),
    substitute(test_proportion_diff(
      vars = "rsp", method = "cmh", variables = list(strata = "strat")
    )),
    substitute(build_table(df = dta))
  )
  expect_identical(result, expected)
})

test_that("add_expr manages expression list which can be used by pipe_expr", {
  lyt <- list()
  lyt <- add_expr(lyt, substitute(basic_table()))
  lyt <- add_expr(
    lyt, substitute(split_cols_by(var = arm), env = list(armcd = "ARMCD"))
  )
  lyt <- add_expr(
    lyt,
    substitute(
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      )
    )
  )

  lyt <- add_expr(lyt, substitute(build_table(df = dta)))
  result <- pipe_expr(lyt)
  expected <- substitute(
    basic_table() %>%
      split_cols_by(var = arm) %>%
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      ) %>%
      build_table(df = dta)
  )
  expect_identical(result, expected)
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

test_that("bracket_expr concatenates expressions into a single expression", {
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
  expect_identical(result, expected)
})

test_that("bracket_expr returns a single evaluable expression", {
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
  expect_identical(result, expected)
})


# prepare_arm ----
test_that("prepare_arm with standard inputs", {
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

  expect_equal(result, expected)
})

test_that("prepare_arm combine ref arms", {
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

  expect_equal(result, expected)
})

test_that("prepare_arm combine ref arms and use new level", {
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

  expect_equal(result, expected)
})

test_that("prepare_arm does not do anything when we don't want to compare or drop arms", {
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

  expect_equal(result, expected)

  result2 <- prepare_arm(
    dataname = "adrs",
    arm_var = "ARMCD",
    ref_arm = "BLA",
    compare_arm = FALSE,
    comp_arm = c("ARM C"),
    drop = FALSE
  )
  expect_identical(result, result2)
})

# prepare_arm_levels ----
test_that("prepare_arm_levels with standard inputs", {
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

  expect_equal(result, expected)
})

test_that("prepare_arm_levels can use parentname arm levels", {
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

  expect_equal(result, expected)
})

test_that("color_lab_values main", {
  vals <- c("3 HIGH", "2 NORMAL", "5 HIGH", "4", "0 LOW")

  expect_identical(
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

test_that("color_lab_values neutral for none characters", {
  vals <- 1:5

  expect_identical(color_lab_values(vals), vals)

  expect_identical(color_lab_values(letters), letters)

})

test_that("clean_description", {
  vals <- rownames(mtcars)

  expect_identical(clean_description(vals), vals)

  vals2 <- 1:10

  expect_identical(clean_description(vals2), as.character(vals2))

})



test_that("get_g_forest_obj_var_name", {
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

  input <- list("paramcd-dataset_ADSL_singleextract-filter1-vals" =
                  c("OS-1")
                  )

  result <- get_g_forest_obj_var_name(paramcd, input)

  expected <- "os1"

  expect_equal(result, expected)
})
