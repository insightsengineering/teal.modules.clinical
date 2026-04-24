describe("migrate_value_choices_to_picks", {
  it("returns a teal.picks::values() object", {
    withr::local_options(lifecycle_verbosity = "quiet")
    output <- migrate_value_choices_to_picks(
      choices_selected(value_choices(teal.data::rADTTE, "PARAMCD")), multiple = FALSE
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
