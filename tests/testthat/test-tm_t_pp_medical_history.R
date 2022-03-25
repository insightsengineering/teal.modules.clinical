testthat::test_that("template_medical_history outputs a list with one named element", {
  testthat::expect_silent(res <- template_medical_history("ANL", "mhterm", "mhbodsys", "mhdistat"))
  testthat::expect_true(is.list(res))
  testthat::expect_length(res, 1)
  testthat::expect_false(is.null(names(res)))
})

testthat::test_that("template_medical_history's output has element table, which is of class call", {
  testthat::expect_silent(res <- template_medical_history("ANL", "mhterm", "mhbodsys", "mhdistat"))
  testthat::expect_true(is.call(res$table))
})

testthat::test_that("template_medical_history - non-default parameters", {
  testthat::expect_silent(res <- template_medical_history("anl", "mhterm", "mhbodsys", "mhdistat"))
  expected <- list(
    table = quote({
      labels <- formatters::var_labels(anl, fill = FALSE)[c("mhbodsys", "mhterm", "mhdistat")]
      mhbodsys_label <- labels["mhbodsys"]
      result <- anl %>%
        dplyr::select(mhbodsys, mhterm, mhdistat) %>%
        dplyr::arrange(mhbodsys) %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        dplyr::mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        dplyr::distinct() %>%
        `colnames<-`(labels)
      result_without_mhbodsys <- result[, -1]
      result_kbl <- kableExtra::kable(result_without_mhbodsys, table.attr = "style='width:100%;'")
      result_kbl <- result_kbl %>%
        kableExtra::pack_rows(index = table(droplevels(result[[mhbodsys_label]]))) %>%
        kableExtra::kable_styling(bootstrap_options = c("basic"), full_width = TRUE)
      result_kbl
    })
  )

  testthat::expect_equal(res, expected)
})
