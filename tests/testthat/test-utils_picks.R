describe("migrate_value_choices_to_picks", {
  it("returns a teal.picks::picks() object", {
    withr::local_options(lifecycle_verbosity = "quiet")
    output <- migrate_value_choices_to_picks(
      choices_selected(value_choices(teal.data::rADTTE, "PARAMCD")),
      multiple = FALSE
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$variables, "variables")
    expect_s3_class(output$values, "values")
    expect_null(output$datasets)
  })

  it("returns the same picks object if input is already a picks object", {
    withr::local_options(lifecycle_verbosity = "quiet")
    input <- teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("AEDECOD", "AESTDTC"), multiple = TRUE),
      check_dataset = FALSE
    )
    output <- migrate_value_choices_to_picks(input)
    expect_identical(output, input)
  })

  it("adds values() when ommited", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(teal.picks::variables("PARAMCD", "PARAMCD"), check_dataset = FALSE)
    )
    expect_s3_class(output$values, "values")
  })

  it("supports picks with dataset, variables and values", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(
        teal.picks::datasets("ADSL", "ADSL"),
        teal.picks::variables("PARAMCD", "PARAMCD"),
        teal.picks::values(c("AEDECOD", "AESTDTC"), multiple = TRUE)
      )
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$datasets, "datasets")
    expect_s3_class(output$variables, "variables")
    expect_s3_class(output$values, "values")
  })

  it("suports picks with datasets and variables but no values", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(
        teal.picks::datasets("ADSL", "ADSL"),
        teal.picks::variables("PARAMCD", "PARAMCD")
      )
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$datasets, "datasets")
    expect_s3_class(output$variables, "variables")
    expect_s3_class(output$values, "values")
  })

  it("suports picks with datasets and variables but no values (and without adding values)", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(
        teal.picks::datasets("ADSL", "ADSL"),
        teal.picks::variables("PARAMCD", "PARAMCD")
      ),
      add_values = FALSE
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$datasets, "datasets")
    expect_s3_class(output$variables, "variables")
    expect_null(output$values)
  })


  it("supports picks with variables and values, but no datasets", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        teal.picks::values(c("AEDECOD", "AESTDTC"), multiple = TRUE),
        check_dataset = FALSE
      )
    )
    expect_s3_class(output, "picks")
    expect_null(output$datasets)
    expect_s3_class(output$variables, "variables")
    expect_s3_class(output$values, "values")
  })

  it("supports picks with only variables", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        check_dataset = FALSE
      )
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$variables, "variables")
    expect_s3_class(output$values, "values")
  })

  it("supports picks with only variables (and does not add values)", {
    output <- migrate_value_choices_to_picks(
      teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        check_dataset = FALSE
      ),
      add_values = FALSE
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$variables, "variables")
    expect_null(output$values)
  })

  it("supports only variables()", {
    output <- migrate_value_choices_to_picks(
      teal.picks::variables("PARAMCD", "PARAMCD")
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$variables, "variables")
    expect_s3_class(output$values, "values")
  })

  it("supports only variables() without adding values", {
    output <- migrate_value_choices_to_picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      add_values = FALSE
    )
    expect_s3_class(output, "picks")
    expect_s3_class(output$variables, "variables")
    expect_null(output$values)
  })
})

describe("migrate_choices_selected_to_values", {
  it("returns a teal.picks::values() object from a choices_selected", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- choices_selected(choices = c("A", "B", "C"), selected = "A")
    output <- migrate_choices_selected_to_values(cs)
    expect_s3_class(output, "values")
    expect_equal(attr(output, "multiple"), FALSE)
  })

  it("returns the same picks object if input already inherits picks", {
    input <- teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("A", "B"), "A"),
      check_dataset = FALSE
    )
    output <- migrate_choices_selected_to_values(input)
    expect_identical(output, input)
  })

  it("respects the multiple argument", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- choices_selected(choices = c("A", "B", "C"), selected = "A")
    output <- migrate_choices_selected_to_values(cs, multiple = TRUE)
    expect_true(attr(output, "multiple"))
  })

  it("infers multiple = TRUE when more than one value is selected", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- choices_selected(choices = c("A", "B", "C"), selected = c("A", "B"))
    output <- migrate_choices_selected_to_values(cs)
    expect_true(attr(output, "multiple"))
  })

  it("preserves fixed = TRUE from the choices_selected object", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- choices_selected(choices = c("A", "B"), selected = "A", fixed = TRUE)
    output <- migrate_choices_selected_to_values(cs)
    expect_true(attr(output, "fixed"))
  })

  it("errors on delayed choices_selected", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- choices_selected(choices = variable_choices("ADSL"), selected = "STUDYID")
    expect_error(
      migrate_choices_selected_to_values(cs),
      "Delayed.*cannot be coerced"
    )
  })

  it("errors when input is not a picks or choices_selected object", {
    expect_error(
      migrate_choices_selected_to_values("something else"),
      class = "error"
    )
  })
})

describe("migrate_choices_selected_to_variables", {
  it("returns the same picks object if input already inherits picks", {
    input <- teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("A", "B"), "A"),
      check_dataset = FALSE
    )
    output <- migrate_choices_selected_to_variables(input)
    expect_identical(output, input)
  })

  it("returns NULL when null.ok = TRUE and x is NULL", {
    output <- migrate_choices_selected_to_variables(NULL, null.ok = TRUE)
    expect_null(output)
  })

  it("errors when x is NULL and null.ok = FALSE", {
    expect_error(
      migrate_choices_selected_to_variables(NULL, null.ok = FALSE)
    )
  })

  it("emits a deprecation warning for choices_selected input", {
    cs <- choices_selected(choices = c("A", "B", "C"), selected = "A")
    lifecycle::expect_deprecated(
      migrate_choices_selected_to_variables(cs)
    )
  })

  it("emits a deprecation warning for select_spec input", {
    ss <- select_spec(choices = c("A", "B", "C"), selected = "A")
    expect_warning(
      migrate_choices_selected_to_variables(ss),
      class = "lifecycle_warning_deprecated"
    )
  })
})

describe("create_picks_helper", {
  it("returns x unchanged if it already has datasets attached", {
    x <- teal.picks::picks(
      datasets("ADSL", "ADSL"),
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("A", "B"), "A"),
      check_dataset = FALSE
    )
    output <- create_picks_helper(datasets = datasets("ADSL", "ADSL"), x = x)
    expect_identical(output, x)
  })

  it("errors when datasets is NULL", {
    x <- teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("A", "B"), "A"),
      check_dataset = FALSE
    )
    expect_error(
      create_picks_helper(datasets = NULL, x = x),
      class = "error"
    )
  })

  it("errors when datasets is not a 'datasets' object", {
    x <- teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("A", "B"), "A"),
      check_dataset = FALSE
    )
    expect_error(
      create_picks_helper(datasets = list(), x = x),
      class = "error"
    )
  })

  it("errors when x is neither pick nor picks", {
    expect_error(
      create_picks_helper(datasets = datasets("ADSL", "ADSL"), x = "not_a_pick"),
      class = "error"
    )
  })

  it("wraps a picks object (without datasets) with the supplied datasets", {
    x <- teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("A", "B"), "A"),
      check_dataset = FALSE
    )
    output <- create_picks_helper(datasets = datasets("ADSL", "ADSL"), x = x)
    expect_s3_class(output, "picks")
    expect_identical(output$datasets, datasets("ADSL", "ADSL"))
  })

  it("wraps a pick object with the supplied datasets", {
    x <- teal.picks::variables("PARAMCD", "PARAMCD")
    output <- create_picks_helper(datasets = datasets("ADSL", "ADSL"), x = x)
    expect_s3_class(output, "picks")
    expect_identical(output$datasets, datasets("ADSL", "ADSL"))
  })
})
