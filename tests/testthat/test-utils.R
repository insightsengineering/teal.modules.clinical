
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
    expr1 = substitute(df),
    expr2 = substitute(head)
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
