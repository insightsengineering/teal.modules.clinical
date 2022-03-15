#' Template: Event rates adjusted for patient-years
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis.
#' @param events_var (`integer`)\cr number of observed events.
#' @param label_paramcd (`string`)\cr title of table based on paramcd
#'
#' @seealso [tm_t_events_patyear()]
#' @keywords internal
#'
template_events_patyear <- function(dataname,
                                    parentname,
                                    arm_var,
                                    events_var,
                                    label_paramcd,
                                    aval_var = "AVAL",
                                    add_total = TRUE,
                                    control = control_incidence_rate(),
                                    drop_arm_levels = TRUE,
                                    basic_table_args = teal.widgets::basic_table_args()) {
  # initialize
  y <- list()
  # data
  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df,
      env = list(df = as.name(dataname))
    )
  )
  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- df_explicit_na(dataname, na_level = ""),
      env = list(dataname = as.name("anl"))
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname, na_level = ""),
      env = list(parentname = as.name(parentname))
    )
  )

  y$data <- bracket_expr(data_list)

  # layout
  layout_list <- list()

  basic_title <- paste0("Event rates adjusted for patient-years by: ", label_paramcd)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = basic_title)
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = expr_basic_table_args %>%
        rtables::split_cols_by(var = arm_var) %>%
        rtables::add_colcounts(),
      env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
    )
  )
  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::add_overall_col(label = "All Patients")
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
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
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adaette <- synthetic_cdisc_data("latest")$adaette
#' adaette <- adaette %>%
#'   dplyr::filter(PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
#'   dplyr::mutate(is_event = CNSR == 0) %>%
#'   dplyr::mutate(n_events = as.integer(is_event))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAETTE", adaette),
#'     code =
#'       "adsl <- synthetic_cdisc_data('latest')$adsl
#'       adaette <- synthetic_cdisc_data('latest')$adaette
#'       adaette <- adaette %>%
#'         dplyr::filter(PARAMCD %in% c('AETTE1', 'AETTE2', 'AETTE3')) %>%
#'         dplyr::mutate(is_event = CNSR == 0) %>%
#'         dplyr::mutate(n_events = as.integer(is_event))"
#'   ),
#'   modules = modules(
#'     tm_t_events_patyear(
#'       label = "AE rate adjusted for patient-years at risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ARMCD")),
#'         selected = "ARMCD"
#'       ),
#'       add_total = TRUE,
#'       events_var = choices_selected(
#'         choices = variable_choices(adaette, "n_events"),
#'         selected = "n_events",
#'         fixed = TRUE
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adaette, "PARAMCD", "PARAM"),
#'         selected = "AETTE1"
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_patyear <- function(label,
                                dataname,
                                parentname = ifelse(
                                  inherits(arm_var, "data_extract_spec"),
                                  teal.transform::datanames_input(arm_var),
                                  "ADSL"
                                ),
                                arm_var,
                                events_var,
                                paramcd,
                                aval_var = choices_selected(
                                  variable_choices(dataname, "AVAL"), "AVAL",
                                  fixed = TRUE
                                ),
                                avalu_var = choices_selected(
                                  variable_choices(dataname, "AVALU"), "AVALU",
                                  fixed = TRUE
                                ),
                                add_total = TRUE,
                                conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                                drop_arm_levels = TRUE,
                                pre_output = NULL,
                                post_output = NULL,
                                basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_events_patyear")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(events_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(avalu_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

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
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_events_patyear <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var, a$paramcd, a$aval_var, a$avalu_var, a$events_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("patyear_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "aval_var", "avalu_var", "events_var")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select an Event Type Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("events_var"),
        label = "Event Variable",
        data_extract_spec = a$events_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu_var"),
        label = "Analysis Unit Variable",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      teal.widgets::optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal (rate)", "Normal (log rate)", "Exact", "Bayr's method"),
        selected = "Normal (rate)",
        multiple = FALSE,
        fixed = FALSE
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          teal.widgets::optionalSelectInput(
            ns("time_unit_output"),
            "Time Unit for AE Rate (in Patient-Years)",
            choices = c(0.1, 1, 10, 100, 1000),
            selected = 100,
            multiple = FALSE,
            fixed = FALSE
          ),
          selectInput(
            ns("time_unit_input"),
            "Analysis Unit",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          )
        )
      )
    ),
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_events_patyear <- function(id,
                               datasets,
                               dataname,
                               parentname,
                               arm_var,
                               paramcd,
                               aval_var,
                               avalu_var,
                               events_var,
                               add_total,
                               drop_arm_levels,
                               label,
                               basic_table_args) {
  stopifnot(is_cdisc_data(datasets))
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    observeEvent(anl_merged(), {
      data <- anl_merged()$data()
      aval_unit_var <- anl_merged()$columns_source$avalu_var
      if (length(aval_unit_var) > 0) {
        choices <- stats::na.omit(unique(data[[aval_unit_var]]))
        choices <- gsub("s$", "", tolower(choices))

        updateSelectInput(
          session,
          "time_unit_input",
          choices = choices,
          selected = choices[1]
        )
      }
    })

    anl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        avalu_var = avalu_var,
        events_var = events_var
      ),
      merge_function = "dplyr::inner_join"
    )

    adsl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var),
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
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

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
        need(checkmate::test_string(input_aval_var), "`Analysis Variable` should be a single column."),
        need(checkmate::test_string(input_events_var), "Events variable should be a single column."),
        need(input$conf_method, "`CI Method` field is not selected."),
        need(input$time_unit_output, "`Time Unit for AE Rate (in Patient-Years)` field is empty."),
        need(
          input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]],
          "`Select an Event Type Parameter is not selected."
        ),
        need(
          !any(is.na(anl_m$data()[[input_events_var]])),
          "`Event Variable` for selected parameter includes NA values."
        )
      )

      NULL
    })

    # The R-code corresponding to the analysis.
    call_preparation <- reactive({
      validate_checks()

      teal.code::chunks_reset()
      anl_m <- anl_merged()
      teal.code::chunks_push_data_merge(anl_m)
      teal.code::chunks_push_new_line()

      anl_adsl <- adsl_merged()
      teal.code::chunks_push_data_merge(anl_adsl)
      teal.code::chunks_push_new_line()

      ANL <- teal.code::chunks_get_var("ANL") # nolint
      label_paramcd <- get_paramcd_label(ANL, paramcd)

      my_calls <- template_events_patyear(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        aval_var = as.vector(anl_m$columns_source$aval_var),
        events_var = as.vector(anl_m$columns_source$events_var),
        label_paramcd = label_paramcd,
        add_total = input$add_total,
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
          time_unit_input = if (input$time_unit_input %in% c("day", "week", "month", "year")) {
            input$time_unit_input
          } else {
            "year"
          },
          time_unit_output = as.numeric(input$time_unit_output)
        ),
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )
      mapply(expression = my_calls, teal.code::chunks_push)
    })

    # Outputs to render.
    patyear_table <- reactive({
      call_preparation()
      teal.code::chunks_safe_eval()
      teal.code::chunks_get_var("result")
    })

    teal.widgets::table_with_settings_srv(
      id = "patyear_table",
      table_r = patyear_table
    )

    # Render R code.
    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(
        list(arm_var, paramcd, aval_var, events_var)
      ),
      modal_title = "Event Rate adjusted for patient-year at risk",
      code_header = label
    )
  })
}
