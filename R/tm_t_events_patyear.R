#' Template: Event rates adjusted for patient-years
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis.
#' @param events_var (`integer`)\cr number of observed events.
#'
#' @seealso [tm_t_events_patyear()]
#'
template_events_patyear <- function(dataname,
                                    parentname,
                                    arm_var,
                                    events_var,
                                    aval_var = "AVAL",
                                    control = control_incidence_rate(),
                                    add_total = TRUE) {
  # initialize
  y <- list()

  # data
  y$data <- substitute(
    expr = anl <- df,
    env = list(df = as.name(dataname))
  )

  # layout
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = basic_table() %>%
        split_cols_by(var = arm_var) %>%
        add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )
  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        add_overall_col(label = "All Patients")
      )
    )
  }
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = estimate_incidence_rate(
        vars = aval_var,
        n_events = events_var,
        control = control_incidence_rate(
          conf_level = conf_level,
          conf_type = conf_type,
          time_unit_input = time_unit_input,
          time_unit_output = time_unit_output
        )
      ),
      env = list(
        aval_var = aval_var,
        events_var = events_var,
        conf_level = control$conf_level,
        conf_type = control$conf_type,
        time_unit_input = control$time_unit_input,
        time_unit_output = control$time_unit_output
      )
    )
  )
  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # table
  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(parent = as.name(parentname))
  )

  y

}

#' Teal module: Event rates adjusted for patient-years
#'
#' @inheritParams module_arguments
#' @param avalu_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices and preselected option for the analysis unit variable.
#' @param events_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all event counts.
#'
#' @export
#' @examples
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' adsl <- radsl(cached = TRUE)
#' adaette <- radaette(cached = TRUE)
#' adaette <- adaette %>%
#'   dplyr::mutate(is_event = CNSR == 0) %>%
#'   dplyr::mutate(n_events = as.integer(is_event))
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAETTE", adaette),
#'     code =
#'       "ADSL <- radsl(cached = TRUE)
#'       ADAETTE <- radaette(cached = TRUE)
#'       ADAETTE <- ADAETTE %>%
#'         dplyr::mutate(is_event = CNSR == 0) %>%
#'         dplyr::mutate(n_events = as.integer(is_event))"
#'   ),
#'   modules = root_modules(
#'     tm_t_events_patyear(
#'       label = "AE rate adjusted for patient-years at risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ARMCD")),
#'         selected = "ARMCD"),
#'       events_var = choices_selected(
#'         choices = variable_choices(adaette, "n_events"),
#'        selected = "n_events",
#'        fixed = TRUE),
#'       paramcd = choices_selected(
#'         choices = value_choices(adaette, "PARAMCD", "PARAM"),
#'         selected = "AETTE1")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_patyear <- function(label,
                                dataname,
                                parentname = ifelse(
                                  is(arm_var, "data_extract_spec"),
                                  datanames_input(arm_var),
                                  "ADSL"
                                ),
                                arm_var,
                                events_var,
                                paramcd,
                                aval_var = choices_selected(
                                  variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE
                                ),
                                avalu_var = choices_selected(
                                  variable_choices(dataname, "AVALU"), "AVALU", fixed = TRUE
                                ),
                                conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                                pre_output = NULL,
                                post_output = NULL
                                ) {
  stop_if_not(
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
      ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
      )
    )

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname),
    events_var = cs_to_des_select(events_var, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_events_patyear,
    ui_args = c(data_extract_list, args),
    server = srv_events_patyear,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_events_patyear <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$paramcd, a$aval_var, a$avalu_var, a$events_var)

  standard_layout(
    output = white_small_well(uiOutput(ns("patyear_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "paramcd", "aval_var", "avalu_var", "events_var")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select an Event Type Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("avalu_var"),
        label = "Analysis Unit Variable",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("events_var"),
        label = "Event Variable",
        data_extract_spec = a$events_var,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("time_unit_input"),
        "Analysis Unit",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      optionalSelectInput(
        ns("time_unit_output"),
        "Time Unit for AE Rate (in Patient-Years)",
        choices = c(0.1, 1, 10, 100, 1000),
        selected = 100,
        multiple = FALSE,
        fixed = FALSE
      ),
      optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal (rate)", "Normal (log rate)", "Exact", "Bayr's method"),
        selected = "Normal (rate)",
        multiple = FALSE,
        fixed = FALSE
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' @noRd
srv_events_patyear <- function(input,
                               output,
                               session,
                               datasets,
                               dataname,
                               parentname,
                               arm_var,
                               paramcd,
                               aval_var,
                               avalu_var,
                               events_var,
                               label) {
  init_chunks()
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)
    avalu_choices <- anl %>%
      select(as.name(avalu_var$select$selected)) %>%
      unique() %>%
      arrange() %>%
      pull()
    updateSelectInput(
      session, "time_unit_input",
      choices = avalu_choices,
      selected = avalu_choices[1]
    )
  })

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, aval_var, avalu_var, events_var),
    input_id = c("arm_var", "paramcd", "aval_var", "avalu_var", "events_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_avalu_var <- as.vector(anl_m$columns_source$avalu_var)
    input_events_var <- as.vector(anl_m$columns_source$events_var)
    input_paramcd <- unlist(paramcd$filter)["vars"]

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_events_var, input_aval_var, input_avalu_var),
      arm_var = input_arm_var
    )

    validate(need(
      input$conf_level > 0 && input$conf_level < 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(
      need(is_character_single(input_aval_var), "`Analysis Variable` should be a single column."),
      need(is_character_single(input_events_var), "Events variable should be a single column."),
      need(input$conf_method, "`CI Method` field is not selected"),
      need(input$time_unit_output, "`Time Unit for AE Rate (in Patient-Years)` field is empty")
    )

    NULL
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    my_calls <- template_events_patyear(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      aval_var = as.vector(anl_m$columns_source$aval_var),
      events_var = as.vector(anl_m$columns_source$events_var),
      control = control_incidence_rate(
        conf_level = as.numeric(input$conf_level), # nolint
        conf_type = if (input$conf_method == "Normal (rate)") {
          "normal"
        } else if (input$conf_method == "Normal (log rate)") {
          "normal_log"
        } else if (input$conf_method == "Exact") {
          "exact"
        } else {
          "byar"
        },
        time_unit_input = if (as.character(input$time_unit_input) == "DAYS") {
          "day"
        } else if (as.character(input$time_unit_input) == "MONTHS") {
          "month"
        } else{
          "year"
        },
        time_unit_output = as.numeric(input$time_unit_output)
      ),
      add_total = input$add_total
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$patyear_table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, paramcd, aval_var, events_var)
    ),
    modal_title = "Event Rate adjusted for patient-year at risk",
    code_header = label
  )
}
