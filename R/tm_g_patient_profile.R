#' Template: Basic Info
#'
#' Creates a basic info template.
#'
#' @inheritParams template_arguments
#' @param patient_id Patient ID to be used.
#' @param binf_vars Variables to be shown in Basic Info tab.
#' @param mhist_vars Medical history variables.
#'
#'
template_basic_info <- function(dataname,
                                patient_id,
                                binf_vars) {
  assert_that(
    is.string(dataname)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr =
      result <- # compared to the original app, ETHNIC, DTHCAUS and DTHADY are not available in ADSL
        dataname %>%
        select(binf_vars) %>%
        gather() %>%
        mutate(key = get_labels(dataname)$column_labels[binf_vars]) %>%
        rename(`   ` = key, ` ` = value),
      env = list(
        dataname = as.name(dataname),
        binf_vars = binf_vars
      )
    )
  )
  # l_html_sub of the ADSUB dataset ris not present in random.cdisc.data
  y$table <- bracket_expr(table_list)

  y
}

#' Template: Medical History
#'
#' Creates medical history template.
#'
#' @inheritParams template_arguments
#'
template_medical_history <- function(dataname,
                                     patient_id,
                                     mhist_vars) {
  assert_that(
    is.string(dataname)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      result <- # compared to the original app, MHDISTAT is not available in ADHM
        dataname %>%
        select(mhist_vars) %>%
        arrange(MHBODSYS) %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        distinct() %>%
        `colnames<-`(get_labels(dataname)$column_labels[mhist_vars])
      }, env = list(
        dataname = as.name(dataname),
        mhist_vars = mhist_vars
      )
    )
  )

  y$table <- bracket_expr(table_list)

  y
}

#' Template: Prior Medication
#'
#' Creates a prior medication template.
#'
#' @inheritParams template_arguments
#'
template_prior_medication <- function(dataname,
                                      patient_id,
                                      pmed_vars,
                                      vital_vars,
                                      vitals_xaxis,
                                      aval) {
  assert_that(
    is.string(dataname)
  )

  y <- list()
  y$table <- list()

  # CMINDC, CMINDC, CMSTDY are not found in ADCM
  table_list <- add_expr(
    list(),
    substitute(expr = {
      result <- # compared to the original app, MHDISTAT is not available in ADSL
        dataname %>%
        filter(ATIREL %in% c("PRIOR", "PRIOR_CONCOMITANT")) %>%
        select(pmed_vars) %>% # remove ATIREL
        filter(!is.na(CMDECOD)) %>%
        distinct() %>%
        `colnames<-`(get_labels(dataname)$column_labels[pmed_vars])
    }, env = list(
      dataname = as.name(dataname),
      pmed_vars = pmed_vars
    ))
  )
  # Note: l_html_concomitant_adcm is still not included since one column is available out of 9

  y$table <- bracket_expr(table_list)
  y
}

#' Template: Vitals
#'
#' Creates a vitals template.
#'
#' @inheritParams template_arguments
#'
template_vitals <- function(dataname,
                            patient_id,
                            pmed_vars,
                            vital_vars,
                            vitals_xaxis,
                            aval,
                            max_day) {
  assert_that(
    is.string(dataname)
  )

  y <- list()
  y$plot <- list()

  vital_plot <- add_expr(
    list(),
    substitute(expr = {
      vitals <-
        dataname %>%
        group_by(vital_vars, vitals_xaxis) %>%
        filter(PARAMCD %in% c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP")) %>%
        summarise(
          AVAL = max(aval, na.rm = T)
        )

      result_plot <- ggplot(data = vitals, mapping = aes(x = vitals_xaxis)) + # replaced VSDY
        geom_ribbon(
          data = vitals %>% tidyr::pivot_wider(names_from = "PARAMCD", values_from = "AVAL"),
          aes(ymin = DIABP, ymax = SYSBP), fill = "red", alpha = 0.1
        ) +
        ggplot2::geom_line(
          data = vitals,
          mapping = ggplot2::aes(y = aval, color = vital_vars),
          size = 1.5,
          alpha = 0.5
        ) +
        scale_color_manual(
          values = c(
            "SYSBP" = "darkred",
            "DIABP" = "red",
            "TEMP" = "purple",
            "RESP" = "cadetblue"
          ) # removed  "PUL", "OXYSAT", "WGHT"
        ) +
        geom_hline(yintercept = 38, color = "purple", linetype = 2, alpha = 0.5, size = 1) +
        geom_text(
          aes(x = 1, y = 38),
          label = "38\u00B0 C",
          color = "purple",
          alpha = 1,
          nudge_y = 2.2
        ) +
        geom_hline(yintercept = 20, color = "cadetblue", linetype = 2, alpha = 0.8, size = 1) +
        geom_text(
          ggplot2::aes(x = 1, y = 20),
          label = "20/min",
          color = "cadetblue",
          alpha = 1,
          nudge_y = 2.2
        ) +
        geom_hline(yintercept = 94, color = "blue", linetype = 2, alpha = 0.5, size = 1) +
        geom_text(
          aes(x = 1, y = 94),
          label = "94%",
          color = "blue",
          alpha = 1,
          nudge_y = 2.2
        ) +
        geom_hline(yintercept = 100, color = "forestgreen", linetype = 2, alpha = 0.5, size = 1) +
        geom_text(
          ggplot2::aes(x = 1, y = 100),
          label = "100bpm",
          color = "forestgreen",
          alpha = 1,
          nudge_y = 2.2
        ) +
        geom_hline(yintercept = 90, color = "red", linetype = 2, alpha = 0.5, size = 1) +
        geom_text(
          ggplot2::aes(x = 1, y = 90),
          label = "90mmHg",
          color = "red",
          alpha = 1,
          nudge_y = -2.2
        ) +
        geom_hline(yintercept = 140, color = "darkred", linetype = 2, alpha = 0.5, size = 1) +
        geom_text(
          aes(x = 1, y = 140),
          label = "140mmHg",
          color = "darkred",
          alpha = 1,
          nudge_y = 2.2
        ) +
        # nolint start
        # geom_text(
        #   aes(
        #     x = max_day,
        #     y = seq(1, max(vitals$AVAL, na.rm = T), 10),
        #     label = seq(1, max(vitals$AVAL, na.rm = T), 10)
        #   ),
        #   color = "black",
        #   alpha = 1,
        #   nudge_y = 2.2
        # ) +
      # nolint end
        scale_x_continuous(
          breaks = seq(1, max(vitals[[vitals_xaxis_char]], na.rm = T), 7), # changed VSDY with ADY
          labels = seq(1, max(vitals[[vitals_xaxis_char]], na.rm = T), 7), # changed VSDY with ADY
          minor_breaks = seq(1, max(vitals[[vitals_xaxis_char]], na.rm = T), 1) # changed VSDY with ADY
        ) +
        scale_y_continuous(
          breaks = seq(0, max(vitals[[vitals_xaxis_char]], na.rm = T), 50),
          name = "Vitals",
          minor_breaks = seq(1, max(vitals[[aval_char]], na.rm = T), 10)
        ) +
        theme_void() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_line(
            size = 0.5,
            linetype = "dotted",
            colour = "grey"
          ),
          panel.grid.minor = element_line(
            size = 0.5,
            linetype = "dotted",
            colour = "grey"
          ),
          legend.position = "top"
        ) +
        xlim(1, max_day)
    }, env = list(
      dataname = as.name(dataname),
      vital_vars = as.name(vital_vars),
      vitals_xaxis = as.name(vitals_xaxis),
      vitals_xaxis_char = vitals_xaxis,
      aval = as.name(aval),
      aval_char = aval,
      max_day = as.numeric(max_day)
    ))
  )

  y$plot <- bracket_expr(vital_plot)
  y
}

#' Template: Therapy
#'
#' Creates a therapy template call.
#'
#' @inheritParams template_arguments
#'
template_therapy <- function(dataname,
                             patient_id) {
  assert_that(
    is.string(dataname)
  )

  y <- list()
  y$table <- list()
  y
}


#' Template: Adverse Events Tab
#'
#' Creates an adverse events template call.
#'
#' @inheritParams template_arguments
#'
template_adverse_events <- function(patient_id,
                                    dataname = "ADAE_FILTERED",
                                    ae_term = "AETERM",
                                    ae_tox_grade = "AETOXGR",
                                    ae_causality = "AEREL",
                                    ae_outcome = "AEOUT",
                                    ae_action = "AEACN",
                                    ae_time = "ASTDY") {
  assert_that(
    is.string(dataname),
    is.string(ae_term),
    is.string(ae_tox_grade),
    is.string(ae_causality),
    is.string(ae_outcome),
    is.string(ae_action),
    is.string(ae_time) || is.null(ae_time)
  )

  y <- list()


  y$table <- list()
  y$chart <- list()

  table_list <- add_expr(
    list(),
    substitute(
      ae_table <- dataname %>%
        select(
          ae_term, ae_tox_grade, ae_causality, ae_outcome, ae_action, ae_time
          ) %>%
        arrange(desc(ae_tox_grade)) %>%
        `colnames<-`(get_labels(dataname)$column_labels[ae_vars]),
      env = list(
        dataname = as.name(dataname),
        ae_term = as.name(ae_term),
        ae_tox_grade = as.name(ae_tox_grade),
        ae_causality = as.name(ae_causality),
        ae_outcome = as.name(ae_outcome),
        ae_action = as.name(ae_action),
        ae_time = as.name(ae_time),
        ae_vars = c(ae_term, ae_tox_grade, ae_causality, ae_outcome, ae_action, ae_time)
      )
    )
  )

  if (is.null(ae_time)) {
    chart_list <- add_expr(
      list(),
      quote(ae_chart <- ggplot2::ggplot())
    )
  } else {
    chart_list <- add_expr(
      list(),
      substitute(
        expr = ae_chart <- dataname %>%
          select(ae_term, ae_time, ae_tox_grade, ae_causality) %>%
          mutate(ATOXGR = as.character(ae_tox_grade)) %>%
          arrange(desc(ATOXGR)) %>%
          mutate(ATOXGR = case_when(
            ATOXGR == "." ~ "UNKNOWN",
            TRUE ~ ATOXGR
          )) %>%
          ggplot2::ggplot(ggplot2::aes(
            fill = ATOXGR, color = ae_term, y = ae_term, x = ae_time)) +
          ggplot2::geom_label(
            ggplot2::aes(label = ae_term),
            color = "black",
            hjust = "left",
            vjust = "bottom",
            size = 3
          ) +
          ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 1.2)) +
          ggplot2::xlim(1, 1.2 * max(dataname[[ae_time_var]])) +
          ggplot2::geom_point(color = "black", size = 2, shape = 24, position = position_nudge(y = -0.15)) +
          ggplot2::ylab("Adverse Events") +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_line(
              size = 0.5,
              linetype = "dotted",
              colour = "grey"),
            panel.grid.minor = element_line(
              size = 0.5,
              linetype = "dotted",
              colour = "grey"),
            legend.position = "top"
          ) +
          theme(legend.position = "none"),
        env = list(
          dataname = as.name(dataname),
          ae_term = as.name(ae_term),
          ae_time = as.name(ae_time),
          ae_tox_grade = as.name(ae_tox_grade),
          ae_causality = as.name(ae_causality),
          ae_time_var = ae_time
        )
      )
    )
  }

  chart_list <- add_expr(
    expr_ls = chart_list,
    new_expr = quote(print(ae_chart))
  )

  y$table <- bracket_expr(table_list)
  y$chart <- bracket_expr(chart_list)

  y
}




#' Teal Module: Patient Profile Teal Module
#'
#' This teal module produces a patient profile report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_id Patient ID column to be used.
#' @param binf_vars variables to be shown in Basic Info tab.
#' @param ae_vars (`choices selected` or `data_extract_input`)\cr Adverse events variables.
#' @param mhist_vars (`choices selected` or `data_extract_input`)\cr Medical history variables.
#' @param pmed_vars (`choices selected` or `data_extract_input`)\cr Prior medication variables.
#' @param vital_vars (`choices selected` or `data_extract_input`)\cr Vitals variables.
#' @param vitals_xaxis (`choices selected` or `data_extract_input`)\cr
#' Time variable to be represented in the vitals plot x-axis.
#' @param aval `AVAL` (`choices selected` or `data_extract_input`)\cr variable.
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#' ADSL <- radsl(cached = TRUE)
#' ADMH <- radmh(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#' ADCM <- radcm(cached = TRUE)
#' ADVS <- radvs(cached = TRUE)
#' ids <- unique(ADSL$USUBJID)
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- radae(cached = TRUE)"),
#'     cdisc_dataset("ADMH", ADMH, code = "ADMH <- radmh(cached = TRUE)"),
#'     cdisc_dataset("ADCM", ADCM, code = "ADCM <- radcm(cached = TRUE)"),
#'     cdisc_dataset("ADVS", ADVS, code = "ADVS <- radvs(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_patient_profile(
#'       label = "Patient Profile",
#'       dataname = "ADSL",
#'       parentname = "ADSL",
#'       plot_height = c(600L, 200L, 2000L),
#'       patient_id = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = ids,
#'           selected = ids[1],
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_vars = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("AETERM", "AETOXGR", "AEREL", "AEOUT", "AEACN", "ASTDY"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       binf_vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL),
#'           selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       mhist_vars = data_extract_spec(
#'         dataname = "ADMH",
#'         select = select_spec(
#'           choices = variable_choices(ADMH),
#'           selected = c("MHBODSYS", "MHTERM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       pmed_vars = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM),
#'           selected = c("CMDECOD","ATIREL"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       vital_vars = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       vitals_xaxis = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS),
#'           selected = c("ADY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       aval = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS),
#'           selected = c("AVAL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }

tm_g_patient_profile <- function(label,
                                 dataname,
                                 parentname = "ADSL",
                                 patient_id,
                                 binf_vars,
                                 mhist_vars,
                                 pmed_vars,
                                 vital_vars,
                                 vitals_xaxis,
                                 aval,
                                 ae_vars,
                                 plot_height = c(700L, 200L, 2000L),
                                 plot_width = c(900L, 200L, 2000L),
                                 pre_output = NULL,
                                 post_output = NULL) {
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  data_extract_list <- list(
    patient_id = cs_to_des_select(patient_id, dataname = parentname),
    binf_vars = cs_to_des_select(binf_vars, dataname = parentname),
    mhist_vars = cs_to_des_select(mhist_vars, dataname = parentname),
    pmed_vars = cs_to_des_select(pmed_vars, dataname = parentname),
    vital_vars = cs_to_des_select(vital_vars, dataname = parentname),
    vitals_xaxis = cs_to_des_select(vitals_xaxis, dataname = parentname),
    ae_vars = cs_to_des_select(ae_vars, dataname = parentname),
    aval = cs_to_des_select(aval, dataname = parentname)
  )

  module(
    label = label,
    ui = ui_g_patient_profile,
    ui_args = c(data_extract_list, args),
    server = srv_g_patient_profile,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    filters = "all"
  )
}

ui_g_patient_profile <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- is_single_dataset(ui_args$patient_id)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel(
          "Basic info",
          div(
            DT::DTOutput(outputId = ns("basic_info_table"))
          )
        ),
        tabPanel(
          "Medical history",
          div(
            DT::DTOutput(outputId = ns("medical_history_table"))
          )
        ),
        tabPanel(
          "Prior medication",
          div(
            DT::DTOutput(outputId = ns("prior_medication_table"))
          )
        ),
        tabPanel(
          "Vitals",
          div(
            plot_with_settings_ui(id = ns("vitals_plot"))
          )
        ),
        tabPanel(
          "Therapy",
          div(
            plot_with_settings_ui(id = ns("therapy"))
          )
        ),
        tabPanel(
          "Adverse events",
          div(
            DT::DTOutput(outputId = ns("ae_table")),
            plot_with_settings_ui(id = ns("ae_chart"))
          )
        ),
        tabPanel(
          "Laboratory values",
          div(
            plot_with_settings_ui(id = ns("lab_values"))
          )
        )
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(ui_args[c("patient_id")]),
      data_extract_input(
        id = ns("patient_id"),
        label = "Select Patient",
        data_extract_spec = ui_args$patient_id,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Basic info'"),
        data_extract_input(
          id = ns("binf_vars"),
          label = "Select variable:",
          data_extract_spec = ui_args$binf_vars,
          is_single_dataset = is_single_dataset_value
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Medical history'"),
        data_extract_input(
          id = ns("mhist_vars"),
          label = "Select variable:",
          data_extract_spec = ui_args$mhist_vars,
          is_single_dataset = is_single_dataset_value
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Prior medication'"),
        list(
          data_extract_input(
            id = ns("pmed_vars"),
            label = "Select variable:",
            data_extract_spec = ui_args$pmed_vars,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Vitals'"),
        list(
          data_extract_input(
            id = ns("vital_vars"),
            label = "Select vital variable:",
            data_extract_spec = ui_args$vital_vars,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("vitals_xaxis"),
            label = "Select vital plot x-axis:",
            data_extract_spec = ui_args$vitals_xaxis,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("aval"),
            label = "Select AVAL variable:",
            data_extract_spec = ui_args$aval,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Adverse events'"),
        data_extract_input(
          id = ns("ae_vars"),
          label = "Select variable:",
          data_extract_spec = ui_args$ae_vars,
          is_single_dataset = is_single_dataset_value
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

srv_g_patient_profile <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  dataname,
                                  parentname,
                                  patient_id,
                                  binf_vars,
                                  mhist_vars,
                                  pmed_vars,
                                  vital_vars,
                                  vitals_xaxis,
                                  aval,
                                  ae_vars,
                                  plot_height,
                                  plot_width,
                                  label) {
  init_chunks()

  # global checks
  validate_checks <- reactive({
    # add checks here
  })

  # Basic Info tab ----
  binf_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(binf_vars),
    input_id = "binf_vars",
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  basic_info_call <- reactive({
    validate_checks()

    call_stack <- chunks$new()
    call_stack_push <- function(...) {
      chunks_push(..., chunks = call_stack)
    }
    chunks_reset()
    chunks_push_data_merge(binf_merged_data())

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`

    call_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))


    my_calls <- template_basic_info(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      binf_vars = input$`binf_vars-dataset_ADSL_singleextract-select`
    )

    mapply(expression = my_calls, call_stack_push)
    call_stack
  })

  output$basic_info_table <- DT::renderDataTable({
    chunks_push_chunks(basic_info_call())
    chunks_safe_eval()
    chunks_get_var("result")
  })

  # Medical history tab ----
  mhist_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(mhist_vars),
    input_id = "mhist_vars",
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  mhist_call <- reactive({
    validate_checks()

    mhist_stack <- chunks$new()
    mhist_stack_push <- function(...) {
      chunks_push(..., chunks = mhist_stack)
    }
    chunks_reset()
    chunks_push_data_merge(mhist_merged_data())

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`

    mhist_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    my_calls <- template_medical_history(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      mhist_vars = input$`mhist_vars-dataset_ADMH_singleextract-select`
    )

    mapply(expression = my_calls, mhist_stack_push)
    mhist_stack
  })

  output$medical_history_table <- DT::renderDataTable({
    chunks_push_chunks(mhist_call())
    chunks_safe_eval()
    chunks_get_var("result")
  })

  # Prior medication tab ----
  pmed_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(pmed_vars, vital_vars, vitals_xaxis, aval),
    input_id = c("pmed_vars", "vital_vars", "vitals_xaxis", "aval"),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  pmed_call <- reactive({
    validate_checks()

    pmed_stack <- chunks$new()
    pmed_stack_push <- function(...) {
      chunks_push(..., chunks = pmed_stack)
    }
    chunks_reset()
    chunks_push_data_merge(pmed_merged_data())

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`

    pmed_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    my_calls <- template_prior_medication(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      pmed_vars = input$`pmed_vars-dataset_ADCM_singleextract-select`,
      vital_vars = input$`vital_vars-dataset_ADVS_singleextract-select`,
      vitals_xaxis = input$`vitals_xaxis-dataset_ADVS_singleextract-select`,
      aval = input$`aval-dataset_ADVS_singleextract-select`
    )

    mapply(expression = my_calls, pmed_stack_push)
    pmed_stack
  })

  output$prior_medication_table <- DT::renderDataTable({
    chunks_push_chunks(pmed_call())
    chunks_safe_eval()
    chunks_get_var("result")
  })

  # Vitals tab ----
  vitals_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(vital_vars, vitals_xaxis, aval),
    input_id = c("vital_vars", "vitals_xaxis", "aval"),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  vitals_call <- reactive({
    validate_checks()

    vitals_stack <- chunks$new()
    vitals_stack_push <- function(...) {
      chunks_push(..., chunks = vitals_stack)
    }
    chunks_reset()
    chunks_push_data_merge(vitals_merged_data())

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`
    vitals_xaxis <- input$`vitals_xaxis-dataset_ADVS_singleextract-select`

    vitals_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
      max_day <- max(ANL_FILTERED[ANL_FILTERED$USUBJID == .(patient_id), ]$ADY, na.rm = T)
    }))

    my_calls <- template_vitals(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      vital_vars = input$`vital_vars-dataset_ADVS_singleextract-select`,
      vitals_xaxis = input$`vitals_xaxis-dataset_ADVS_singleextract-select`,
      aval = input$`aval-dataset_ADVS_singleextract-select`,
      max_day = "max_day"
    )

    mapply(expression = my_calls, vitals_stack_push)
    vitals_stack
  })

  vitals_plot <- reactive({
    chunks_push_chunks(vitals_call())

    chunks_safe_eval()
    chunks_get_var("result_plot")
  })

  # Therapy tab ----
  template_therapy(dataname = dataname, patient_id = patient_id)

  # Adverse events tab ----
  ae_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(ae_vars),
    input_id = "ae_vars",
    anl_name = "ANL"
  )
  ae_calls <- reactive({
    validate(
      need("ADAE" %in% datasets$datanames(), message = "ADAE dataset needed to show Adverse events info.")
    )
    ae_stack <- chunks$new()
    ae_stack$reset()

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`
    ae_stack$push(bquote({
      ADAE_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    ae_calls <- template_adverse_events("ADSL")
    mapply(ae_calls, FUN = function(x) chunks_push(x, chunks = ae_stack))
    ae_stack
  })
  output$ae_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_data_merge(ae_merged_data())
    chunks_push_chunks(ae_calls())
    chunks_safe_eval()
    chunks_get_var("ae_table")
  })

  ae_chart <- reactive({
    chunks_reset()
    chunks_push_data_merge(ae_merged_data())
    chunks_push_chunks(ae_calls())
    chunks_safe_eval()
    chunks_get_var("ae_chart")
  })

  callModule(
    plot_with_settings_srv,
    id = "ae_chart",
    plot_r = ae_chart,
    height = plot_height,
    width = plot_width
  )

  callModule(
    plot_with_settings_srv,
    id = "vitals_plot",
    plot_r = vitals_plot,
    height = plot_height,
    width = plot_width
  )


  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(
      patient_id,
      binf_vars,
      mhist_vars,
      pmed_vars,
      aval,
      vital_vars,
      vitals_xaxis,
      ae_vars)),
    modal_title = label
  )
}
