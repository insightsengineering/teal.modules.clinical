#' Teal Module for `Kaplan-Meier` Plot (teal.picks)
#'
#' @description
#'
#' This teal module produces a grid style `Kaplan-Meier` plot for data with
#' `ADaM` structure.
#'
#' This is a `teal.picks` implementation of [tm_g_km()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
tm_g_km.picks <- function(label,
                          adtte_name,
                          mae_name,
                          adtte_vars = list(
                            aval = "AVAL",
                            is_event = "is_event",
                            paramcd = "PARAMCD",
                            usubjid = "USUBJID",
                            avalu = "AVALU"
                          ),
                          exclude_assays = "counts",
                          summary_funs = list(
                            Mean = colMeans,
                            Median = matrixStats::colMedians,
                            Max = matrixStats::colMaxs
                          ),
                          pre_output = NULL,
                          post_output = NULL,
                          .test = FALSE,
                          transformators = list()) {
  message("Initializing tm_g_km")
  assert_string(label)
  assert_string(adtte_name)
  assert_string(mae_name)
  assert_adtte_vars(adtte_vars)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_km.picks,
    server_args = args[names(args) %in% names(formals(srv_g_km.picks))],
    ui = ui_g_km.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_km.picks))],
    transformators = transformators,
    datanames = c(adtte_name, mae_name)
  )
}

#' @describeIn tm_g_km.picks sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_km.picks <- function(id,
                          adtte_name,
                          mae_name,
                          summary_funs,
                          pre_output,
                          post_output,
                          .test = FALSE) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = tags$div(
      ### Reporter
      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
      tags$br(), tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      uiOutput(ns("experiment_ui")),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      helpText("Analysis of ADTTE:", tags$code(adtte_name)),
      adtteSpecInput(ns("adtte")),
      bslib::accordion(
        bslib::accordion_panel(
          input_id = "settings_item",
          open = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("strata"), "Select Strata"),
          sliderInput(
            ns("percentiles"),
            "Select quantiles to be displayed",
            min = 0,
            max = 1,
            value = c(0, 0.5)
          )
        )
      )
    ),
    output = div(
      if (.test) verbatimTextOutput(ns("table")) else NULL,
      teal.widgets::plot_with_settings_ui(ns("plot"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_km.picks sets up the server.
#' @inheritParams module_arguments
#' @export
srv_g_km.picks <- function(id,
                           data,
                           filter_panel_api,
                           reporter,
                           adtte_name,
                           mae_name,
                           adtte_vars,
                           summary_funs,
                           exclude_assays,
                           .test = FALSE) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  assert_class(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.hermes")

    # -- Validate inputs using teal:::validate_input --
    validated_q <- reactive({
      obj <- req(data())
      validate_input(
        inputId = "adtte-paramcd",
        condition = !is.null(input$`adtte-paramcd`) && nchar(input$`adtte-paramcd`) > 0,
        message = "Please select an endpoint (PARAMCD)."
      )
      validate_input(
        inputId = "genes-gene_var",
        condition = !is.null(input$`genes-gene_var`) && length(input$`genes-gene_var`) > 0,
        message = "Please select at least one gene."
      )
      obj
    })

    output$experiment_ui <- renderUI({
      experimentSpecInput(session$ns("experiment"), data, mae_name)
    })

    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
      mae_name = mae_name,
      sample_vars_as_factors = FALSE
    )
    assay <- assaySpecServer(
      "assay",
      assays = experiment$assays,
      exclude_assays = exclude_assays
    )
    genes <- geneSpecServer(
      "genes",
      funs = summary_funs,
      gene_choices = experiment$genes
    )
    strata <- sampleVarSpecServer(
      "strata",
      experiment_name = experiment$name,
      original_data = experiment$data
    )

    percentiles_without_borders <- reactive({
      percentiles <- input$percentiles
      result <- setdiff(percentiles, c(0, 1))
      validate_input(
        inputId = "percentiles",
        condition = length(result) > 0,
        message = "Please select at least one quantile other than 0 and 1"
      )
      result
    })

    adtte <- adtteSpecServer(
      "adtte",
      data = validated_q,
      adtte_name = adtte_name,
      mae_name = mae_name,
      adtte_vars = adtte_vars,
      experiment_data = strata$experiment_data,
      experiment_name = experiment$name,
      assay = assay,
      genes = genes,
      probs = percentiles_without_borders
    )

    km_data <- reactive({
      strata_var <- strata$sample_var()
      binned_adtte <- adtte$binned_adtte_subset()

      variables <- list(
        tte = adtte_vars$aval,
        is_event = adtte_vars$is_event,
        arm = adtte$gene_factor,
        strat = strata_var
      )

      list(binned_adtte = binned_adtte, variables = variables)
    })

    km_plot <- reactive({
      km_data <- km_data()
      binned_adtte <- km_data$binned_adtte
      variables <- km_data$variables
      tern::g_km(binned_adtte, variables = variables, annot_coxph = TRUE)
    })

    output$km_plot <- renderPlot(km_plot())

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = km_plot
    )

    if (.test) {
      output$table <- renderPrint(km_data())
    }

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template(
          title = "Kaplan-Meier Plot",
          label = label,
          with_filter = TRUE,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Selected Options", "header3")
        encodings_list <- list(
          "Experiment:",
          input$`experiment-name`,
          "\nAssay:",
          input$`assay-name`,
          "\nGenes Selected:",
          paste0(genes()$get_gene_labels(), collapse = ", "),
          "\nGene Summary:",
          input$`genes-fun_name`,
          "\nEndpoint:",
          input$`adtte-paramcd`,
          "\nStrata Selected:",
          input$`strata-sample_var`,
          "\nQuantiles Displayed:",
          paste0(input$percentiles, collapse = "-")
        )
        null_encodings_indices <- which(sapply(encodings_list, function(x) is.null(x) || x == ""))
        final_encodings <- if (length(null_encodings_indices) > 0) {
          null_encodings_indices_1 <- c(null_encodings_indices, null_encodings_indices - 1)
          paste(encodings_list[-null_encodings_indices_1], collapse = " ")
        } else {
          paste(encodings_list, collapse = " ")
        }

        card$append_text(final_encodings, style = "verbatim")
        card$append_text("Plot", "header3")
        card$append_plot(km_plot(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card
      }
      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}

#' @describeIn tm_g_km.picks sample module function.
#' @export
#' @examples
#' if (interactive()) {
#'   sample_tm_g_km.picks()
#' }
sample_tm_g_km.picks <- function(.test = FALSE) { # nolint: object_name_linter.
  data <- within(teal.data::teal_data(), {
    ADTTE <- teal.data::rADTTE |> # nolint: object_name_linter.
      dplyr::mutate(is_event = .data$CNSR == 0)
    MAE <- hermes::multi_assay_experiment # nolint: object_name_linter.
  })
  join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")

  modules <- teal::modules(
    tm_g_km.picks(
      label = "kaplan-meier",
      adtte_name = "ADTTE",
      mae_name = "MAE",
      .test = .test
    )
  )

  app <- teal::init(
    data = data,
    modules = modules
  )

  shinyApp(ui = app$ui, server = app$server)
}
