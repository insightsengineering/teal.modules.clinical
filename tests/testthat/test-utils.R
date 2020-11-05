library(dplyr)
library(random.cdisc.data)
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

adrs <- radrs(cached = TRUE)
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
    c(0L, 73L, 0L, 18L, 44L, 164L, 0L, 101L, 0L, 0L),
    .Dim = c(5L, 2L),
    .Dimnames = structure(
      list(
        c(
          "Complete Response (CR)", "Non-CR or Non-PD (NON CR/PD)",
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

test_that("styled_expr can format expressions", {
  expr <- quote(
    basic_table() %>%
      split_cols_by(var = "ARMCD") %>%
      test_proportion_diff(
        vars = "rsp", method = "cmh", variables = list(strata = "strat")
      ) %>%
      build_table(df = dta)
  )

  result <- capture.output(styled_expr(expr))
  expected <- c(
    "basic_table() %>%",
    "  split_cols_by(var = \"ARMCD\") %>%",
    "  test_proportion_diff(",
    "    vars = \"rsp\",",
    "    method = \"cmh\",",
    "    variables = list(strata = \"strat\")",
    "  ) %>%",
    "  build_table(df = dta)"
  )

  expect_identical(result, expected)
})
