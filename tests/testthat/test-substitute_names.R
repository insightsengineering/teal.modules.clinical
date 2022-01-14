test_that("substitute_q works as expected", {
  result <- substitute_q(
    qexpr = quote(a <- c + d),
    env = list(c = 5, d = 3)
  )
  expected <- quote(a <- 5 + 3)
  expect_identical(result, expected)
})

test_that("substitute_q fails on unquoted call", {
  expect_error(substitute_q(c + d, env = list(c = 5, d = 3)))
})

test_that("h_subst_lhs_names works as expected", {
  result <- h_subst_lhs_names(
    qexpr = quote(c(a = d, b = 3, z = "foo")),
    names = list(a = as.name("x"), z = as.name("y"))
  )
  expected <- quote(c(x = d, b = 3, y = "foo"))
  expect_identical(result, expected)
})

test_that("substitute_lhs_names works as expected with nested expressions", {
  result <- substitute_lhs_names(
    qexpr = quote(anl <- anl %>% mutate(a = factor(a))),
    names = list(a = as.name("AEBODSYS"))
  )
  expected <- quote(anl <- anl %>% mutate(AEBODSYS = factor(a)))
  expect_identical(result, expected)
})

test_that("substitute_lhs_names does not lead to infinite recursion", {
  result <- substitute_lhs_names(
    qexpr = quote(c(a = d, b = 3, z = "foo")),
    names = list(b = as.name("x"), z = as.name("y"))
  )
  expected <- quote(c(a = d, x = 3, y = "foo"))
  expect_identical(result, expected)
})

test_that("substitute_rhs works as expected", {
  result <- substitute_rhs(
    qexpr = quote(c(a = d, b = 3, z = "foo")),
    env = list(d = as.name("x"), z = as.name("y"))
  )
  expected <- quote(c(a = x, b = 3, z = "foo"))
  expect_identical(result, expected)
})

test_that("substitute_names works as expected", {
  result <- substitute_names(
    expr = c(a = d, b = 3, z = "foo"),
    names = list(d = as.name("x"), z = as.name("y"))
  )
  expected <- quote(c(a = x, b = 3, y = "foo"))
  expect_identical(result, expected)
})

test_that("substitute_names can also substitute other expressions if requested", {
  result <- substitute_names(
    expr = c(a = d, b = 3, z = doo),
    names = list(d = as.name("x"), z = as.name("y")),
    others = list(doo = "loo")
  )
  expected <- quote(c(a = x, b = 3, y = "loo"))
  expect_identical(result, expected)
})

test_that("substitute_names works as expected with nested expressions", {
  result <- substitute_names(
    expr = anl <- anl %>% mutate(a = factor(a)),
    names = list(a = as.name("AEBODSYS"))
  )
  expected <- quote(anl <- anl %>% mutate(AEBODSYS = factor(AEBODSYS)))
  expect_identical(result, expected)
})
