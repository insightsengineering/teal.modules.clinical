testthat::test_that("template_smq generates correct expressions with default arguments", {
  result <- template_smq(
    parentname = "adsl",
    dataname = "adae",
    arm_var = c("ARMCD", "SEX"),
    id_var = "USUBJID",
    llt = "AEDECOD",
    add_total = FALSE,
    drop_arm_levels = FALSE,
    na_level = "<Missing>",
    smq_varlabel = "Standardized MedDRA Query",
    baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
    sort_criteria = c("freq_desc")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_smq generates correct expressions with custom arguments", {
  result <- template_smq(
    parentname = "myadsl",
    dataname = "myadae",
    arm_var = "myARMCD",
    id_var = "myUSUBJID",
    llt = "myAEDECOD",
    add_total = FALSE,
    drop_arm_levels = FALSE,
    na_level = "<Missing>",
    smq_varlabel = "mylabel",
    baskets = c("mybaskets"),
    sort_criteria = c("freq_desc")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
