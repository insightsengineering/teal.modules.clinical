create_tm_t_pp_laboratory_module <- function(
  # nolint: object_length_linter.
  # nolint: object_length.
  paramcd = "PARAMCD",
  param = "PARAM",
  time_points = "ADY",
  anrind = "ANRIND",
  aval_var = "AVAL",
  avalu_var = "AVALU"
) {
  tm_t_pp_laboratory(
    label = "Vitals",
    dataname = "ADLB",
    parentname = "ADSL",
    patient_col = "USUBJID",
    paramcd = teal.picks::variables(choices = paramcd, selected = paramcd, multiple = FALSE, fixed = TRUE),
    param = teal.picks::variables(choices = param, selected = param, multiple = FALSE, fixed = TRUE),
    time_points = teal.picks::variables(
      choices = time_points, selected = time_points,
      multiple = FALSE, fixed = TRUE
    ),
    anrind = teal.picks::variables(choices = anrind, selected = anrind, multiple = FALSE, fixed = TRUE),
    aval_var = teal.picks::variables(choices = aval_var, selected = aval_var, multiple = FALSE, fixed = TRUE),
    avalu_var = teal.picks::variables(choices = avalu_var, selected = avalu_var, multiple = FALSE, fixed = TRUE)
  )
}

# Runs a testServer session and returns the table_data from the returned teal_data.
# NOTE: parameter is named `pid` (not `patient_id`) to avoid shadowing the
# `patient_id` reactive binding inside srv_g_laboratory's session data mask.
capture_table_data <- function(mod, pid, data, round_value = "4") {
  result <- NULL
  shiny::testServer(
    mod$server,
    args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
    expr = {
      session$setInputs(patient_id = pid, round_value = round_value)
      result <<- session$returned()[["table_data"]]
    }
  )
  result
}

testthat::describe("tm_t_pp_laboratory server: reactive inputs produce expected changes in returned teal_data", {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADLB <- tmc_ex_adlb
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[c("ADSL", "ADLB")]

  patients <- unique(tmc_ex_adsl[["USUBJID"]])

  testthat::it("updates pt_id in returned teal_data when patient_id input changes", {
    mod <- create_tm_t_pp_laboratory_module()
    shiny::testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(patient_id = patients[[1L]], round_value = "4")
        pt_1 <- session$returned()[["pt_id"]]

        session$setInputs(patient_id = patients[[2L]])
        pt_2 <- session$returned()[["pt_id"]]

        testthat::expect_equal(pt_1, patients[[1L]])
        testthat::expect_equal(pt_2, patients[[2L]])
        testthat::expect_false(identical(pt_1, pt_2))
      }
    )
  })

  testthat::it("produces different table_data in returned teal_data when round_value changes
    from 0 to 4 decimal places", {
    mod <- create_tm_t_pp_laboratory_module()
    shiny::testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(patient_id = patients[[1L]], round_value = "0")
        table_round_0 <- session$returned()[["table_data"]]

        session$setInputs(round_value = "4")
        table_round_4 <- session$returned()[["table_data"]]

        testthat::expect_false(identical(table_round_0, table_round_4))
      }
    )
  })

  testthat::it("produces different table_data when paramcd changes from PARAMCD to STUDYID", {
    table_paramcd <- capture_table_data(create_tm_t_pp_laboratory_module(paramcd = "PARAMCD"), patients[[1L]], data)
    table_studyid <- capture_table_data(create_tm_t_pp_laboratory_module(paramcd = "STUDYID"), patients[[1L]], data)
    testthat::expect_false(identical(table_paramcd, table_studyid))
  })

  testthat::it("produces different table_data when param changes from PARAM to SEX", {
    table_param <- capture_table_data(create_tm_t_pp_laboratory_module(param = "PARAM"), patients[[1L]], data)
    table_sex <- capture_table_data(create_tm_t_pp_laboratory_module(param = "SEX"), patients[[1L]], data)
    testthat::expect_false(identical(table_param, table_sex))
  })

  testthat::it("produces different table_data when time_points changes from ADY to AGE", {
    table_ady <- capture_table_data(create_tm_t_pp_laboratory_module(time_points = "ADY"), patients[[1L]], data)
    table_age <- capture_table_data(create_tm_t_pp_laboratory_module(time_points = "AGE"), patients[[1L]], data)
    testthat::expect_false(identical(table_ady, table_age))
  })

  testthat::it("produces different table_data when avalu_var changes from AVALU to SEX", {
    table_avalu <- capture_table_data(create_tm_t_pp_laboratory_module(avalu_var = "AVALU"), patients[[1L]], data)
    table_sex <- capture_table_data(create_tm_t_pp_laboratory_module(avalu_var = "SEX"), patients[[1L]], data)
    testthat::expect_false(identical(table_avalu, table_sex))
  })

  testthat::it("produces different table_data when aval_var changes from AVAL to AGE", {
    table_aval <- capture_table_data(create_tm_t_pp_laboratory_module(aval_var = "AVAL"), patients[[1L]], data)
    table_age <- capture_table_data(create_tm_t_pp_laboratory_module(aval_var = "AGE"), patients[[1L]], data)
    testthat::expect_false(identical(table_aval, table_age))
  })

  testthat::it("produces different table_data when anrind changes from ANRIND to SEX", {
    table_anrind <- capture_table_data(create_tm_t_pp_laboratory_module(anrind = "ANRIND"), patients[[1L]], data)
    table_sex <- capture_table_data(create_tm_t_pp_laboratory_module(anrind = "SEX"), patients[[1L]], data)
    testthat::expect_false(identical(table_anrind, table_sex))
  })
})
