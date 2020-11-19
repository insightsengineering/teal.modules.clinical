test_that("compare_expr_list works with simple expressions", {
  result <- compare_expr_list(
    x = list(
      quote(a <- 5)
    ),
    y = list(
      str2lang("a <- 5")
    )
  )
  expect_true(result)
})

test_that("compare_expr_list works with complex expressions", {
  result <- compare_expr_list(
    x = list(
      quote({
        a <- 5
        b <- 3
      }),
      quote(a <- mutate(b = filter(c = 3)))
    ),
    y = list(
      bracket_expr(
        list(
          quote(a <- 5),
          substitute(b <- c, env = list(c = 3))
        )
      ),
      str2lang("a <- mutate(b = filter(c = 3))")
    )
  )
  expect_true(result)
})
