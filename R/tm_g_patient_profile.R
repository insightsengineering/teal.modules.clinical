#' Template: Basic Info
#'
#' Creates a basic info template.
#'
#' @inheritParams template_arguments
#' @param patient_id Patient ID to be used.
#' @param binf_vars Variables to be shown in Basic Info tab.
#' @param mhterm Medical history variables.
#'
#'
template_basic_info <- function(dataname,
                                binf_vars) {
  assert_that(
    is.string(dataname),
    is_character_vector(binf_vars)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(
      expr = {
        values <- dataname %>%
          select(binf_vars) %>%
          # we are sure that only one row
          head(1) %>%
          t()

        key <- get_labels(dataname)$column_labels[rownames(values)]

        result <- # compared to the original app, ETHNIC, DTHCAUS and DTHADY are not available in ADSL
          data.frame(key = key, value = values) %>%
          select(key, value) %>%
          rename(`   ` = key, ` ` = value)

      }, env = list(
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
#' @param mhbodsys (`character`)\cr
#' Body system or organ class column.
#'
template_medical_history <- function(dataname,
                                     patient_id,
                                     mhterm = "MHTERM",
                                     mhbodsys = "MHBODSYS") {
  assert_that(
    is.string(dataname),
    is.string(mhterm),
    is.string(mhbodsys)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      result <- # compared to the original app, MHDISTAT is not available in ADHM
        dataname %>%
        select(mhbodsys, mhterm) %>%
        arrange(mhbodsys) %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        distinct() %>%
        `colnames<-`(get_labels(dataname)$column_labels[c(mhbodsys_char, mhterm_char)])
    }, env = list(
      dataname = as.name(dataname),
      mhbodsys = as.name(mhbodsys),
      mhterm = as.name(mhterm),
      mhbodsys_char = mhbodsys,
      mhterm_char = mhterm
    ))
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
                                      atirel = "ATIREL",
                                      medname_decoding = "CMDECOD") {
  assert_that(
    is.string(dataname),
    is.string(atirel),
    is.string(medname_decoding)
  )

  y <- list()
  y$table <- list()

  # CMINDC, CMINDC, CMSTDY are not found in ADCM
  table_list <- add_expr(
    list(),
    substitute(expr = {
      result <-
        dataname %>%
        filter(atirel %in% c("PRIOR", "PRIOR_CONCOMITANT")) %>%
        select(cmdecod) %>% # missing CMINDC, CMINDC, CMSTDY
        filter(!is.na(cmdecod)) %>%
        distinct() %>%
        `colnames<-`(get_labels(dataname)$column_labels[c(atirel_char, cmdecod_char)])
    }, env = list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(medname_decoding),
      atirel_char = atirel,
      cmdecod_char = medname_decoding
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
                            paramcd = "PARAMCD",
                            vitals_xaxis = "ADY",
                            aval = "AVAL") {
  assert_that(
    is.string(dataname),
    is.string(paramcd),
    is.string(vitals_xaxis),
    is.string(aval),
  )
  # Note: VSDY (study day of vital signs) was replaced with ADY (analysis day)
  y <- list()
  y$plot <- list()

  vital_plot <- add_expr(
    list(),
    substitute(expr = {
      vitals <-
        dataname %>%
        group_by(paramcd, vitals_xaxis) %>%
        filter(paramcd %in% c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP")) %>%
        summarise(
          AVAL = max(aval, na.rm = T)
        )

      max_day <- max(vitals[[vitals_xaxis_char]], na.rm = T)
      max_aval <- max(vitals[[aval_char]], na.rm = T)
      max_aval_seq <- seq(0, max_aval, 10)

      result_plot <- ggplot(data = vitals, mapping = aes(x = vitals_xaxis)) + # replaced VSDY
        geom_line(
          data = vitals,
          mapping = aes(y = aval, color = paramcd),
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
          aes(x = 1, y = 20),
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
          aes(x = 1, y = 100),
          label = "100bpm",
          color = "forestgreen",
          alpha = 1,
          nudge_y = 2.2
        ) +
        geom_hline(yintercept = 90, color = "red", linetype = 2, alpha = 0.5, size = 1) +
        geom_text(
          aes(x = 1, y = 90),
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
        geom_text(
          data = data.frame(
            x = rep(max_day, length(max_aval_seq)),
            y = max_aval_seq,
            l = as.character(max_aval_seq)
          ),
          aes(
            x = x,
            y = y,
            label = l
          ),
          color = "black",
          alpha = 1,
          nudge_y = 2.2
        ) +
        scale_x_continuous(
          limits = c(1, max_day)
        ) +
        scale_y_continuous(
          breaks = seq(0, max(vitals[[vitals_xaxis_char]], na.rm = T), 50),
          name = "Vitals",
          minor_breaks = seq(0, max(vitals[[aval_char]], na.rm = T), 10)
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
        )

      print(result_plot)
    }, env = list(
      dataname = as.name(dataname),
      paramcd = as.name(paramcd),
      paramcd_char = paramcd,
      vitals_xaxis = as.name(vitals_xaxis),
      vitals_xaxis_char = vitals_xaxis,
      aval = as.name(aval),
      aval_char = aval
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
      quote(ae_chart <- ggplot())
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
          ggplot(aes(
            fill = ATOXGR, color = ae_term, y = ae_term, x = ae_time
          )) +
          geom_label(
            aes(label = ae_term),
            color = "black",
            hjust = "left",
            vjust = "bottom",
            size = 3
          ) +
          scale_y_discrete(expand = expansion(add = 1.2)) +
          xlim(1, 1.2 * max(dataname[[ae_time_var]])) +
          geom_point(color = "black", size = 2, shape = 24, position = position_nudge(y = -0.15)) +
          ylab("Adverse Events") +
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
#' @param ae_term (`choices selected` or `data_extract_input`)\cr \code{AETERM} column of the ADAE dataset.
#' @param ae_tox_grade (`choices selected` or `data_extract_input`)\cr \code{AETOXGR} column of the ADAE dataset.
#' @param ae_causality (`choices selected` or `data_extract_input`)\cr \code{AEREL} column of the ADAE dataset.
#' @param ae_outcome (`choices selected` or `data_extract_input`)\cr \code{AEOUT} column of the ADAE dataset.
#' @param ae_action (`choices selected` or `data_extract_input`)\cr \code{AEACN} column of the ADAE dataset.
#' @param ae_time (`choices selected` or `data_extract_input`)\cr \code{ASTDY} column of the ADAE dataset.
#' @param mhterm (`choices selected` or `data_extract_input`)\cr \code{MHTERM} column of the ADMH dataset.
#' @param mhbodsys (`choices selected` or `data_extract_input`)\cr \code{MHBODSYS} column of the ADMH dataset.
#' @param paramcd (`choices selected` or `data_extract_input`)\cr \code{PARAMCD} column of the ADVS dataset.
#' @param vitals_xaxis (`choices selected` or `data_extract_input`)\cr
#' Time variable to be represented in the vitals plot x-axis.
#' @param aval `AVAL` (`choices selected` or `data_extract_input`)\cr variable.
#' @param atirel (`choices selected` or `data_extract_input`) \code{ATIREL} column of the ADCM dataset.
#' @param medname_decoding (`choices selected` or `data_extract_input`) \code{CMDECOD} column of the ADCM dataset.
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
#'       binf_vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL),
#'           selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       mhterm = data_extract_spec(
#'         dataname = "ADMH",
#'         select = select_spec(
#'           choices = variable_choices(ADMH),
#'           selected = c("MHTERM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       mhbodsys = data_extract_spec(
#'         dataname = "ADMH",
#'         select = select_spec(
#'           choices = variable_choices(ADMH),
#'           selected = c("MHBODSYS"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       paramcd = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "PARAMCD"),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       vitals_xaxis = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "ADY"),
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
#'       ),
#'       atirel = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM),
#'           selected = c("ATIREL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       medname_decoding = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM),
#'           selected = c("CMDECOD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_term = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("AETERM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_tox_grade = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("AETOXGR"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_causality = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("AEREL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_outcome = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("AEOUT"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_action = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("AEACN"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_time = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE),
#'           selected = c("ASTDY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_patient_profile <- function(label,
                                 dataname,
                                 parentname = "ADSL",
                                 patient_id,
                                 binf_vars = NULL,
                                 mhterm = NULL,
                                 mhbodsys = NULL,
                                 atirel = NULL,
                                 medname_decoding = NULL,
                                 paramcd = NULL,
                                 vitals_xaxis = NULL,
                                 aval = NULL,
                                 ae_term = NULL,
                                 ae_tox_grade = NULL,
                                 ae_causality = NULL,
                                 ae_outcome = NULL,
                                 ae_action = NULL,
                                 ae_time = NULL,
                                 plot_height = c(700L, 200L, 2000L),
                                 plot_width = c(900L, 200L, 2000L),
                                 pre_output = NULL,
                                 post_output = NULL) {
  assert_that(is_character_single(label))
  assert_that(is_character_single(dataname))
  assert_that(is_character_single(parentname))
  assert_that(is.null(pre_output) || is(pre_output, "shiny.tag"),
    msg = "pre_output should be either null or shiny.tag type of object")
  assert_that(is.null(post_output) || is(post_output, "shiny.tag"),
    msg = "post_output should be either null or shiny.tag type of object")

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  data_extract_list <- list(
    patient_id = cs_to_des_select(patient_id, dataname = parentname),
    binf_vars = if_not_null(binf_vars, cs_to_des_select(binf_vars, dataname = parentname)),
    mhterm = if_not_null(mhterm, cs_to_des_select(mhterm, dataname = parentname)),
    mhbodsys = if_not_null(mhbodsys, cs_to_des_select(mhbodsys, dataname = parentname)),
    paramcd = if_not_null(paramcd, cs_to_des_select(paramcd, dataname = parentname)),
    vitals_xaxis = if_not_null(vitals_xaxis, cs_to_des_select(vitals_xaxis, dataname = parentname)),
    aval = if_not_null(aval, cs_to_des_select(aval, dataname = parentname)),
    atirel = if_not_null(atirel, cs_to_des_select(atirel, dataname = parentname)),
    medname_decoding = if_not_null(medname_decoding, cs_to_des_select(medname_decoding, dataname = parentname)),
    ae_term = if_not_null(ae_term, cs_to_des_select(ae_term, dataname = parentname)),
    ae_tox_grade = if_not_null(ae_tox_grade, cs_to_des_select(ae_tox_grade, dataname = parentname)),
    ae_causality = if_not_null(ae_causality, cs_to_des_select(ae_causality, dataname = parentname)),
    ae_outcome = if_not_null(ae_outcome, cs_to_des_select(ae_outcome, dataname = parentname)),
    ae_action = if_not_null(ae_action, cs_to_des_select(ae_action, dataname = parentname)),
    ae_time = if_not_null(ae_time, cs_to_des_select(ae_time, dataname = parentname))
  )
  assert_that(is.cs_or_des(patient_id))
  assert_that(is.null(binf_vars) || is.cs_or_des(binf_vars))
  assert_that(is.null(mhterm) || is.cs_or_des(mhterm))
  assert_that(is.null(mhbodsys) || is.cs_or_des(mhbodsys))
  assert_that(is.null(paramcd) || is.cs_or_des(paramcd))
  assert_that(is.null(vitals_xaxis) || is.cs_or_des(vitals_xaxis))
  assert_that(is.null(aval) || is.cs_or_des(aval))
  assert_that(is.null(atirel) || is.cs_or_des(atirel))
  assert_that(is.null(medname_decoding) || is.cs_or_des(medname_decoding))
  assert_that(is.null(ae_term) || is.cs_or_des(ae_term))
  assert_that(is.null(ae_tox_grade) || is.cs_or_des(ae_tox_grade))
  assert_that(is.null(ae_causality) || is.cs_or_des(ae_causality))
  assert_that(is.null(ae_outcome) || is.cs_or_des(ae_outcome))
  assert_that(is.null(ae_action) || is.cs_or_des(ae_action))
  assert_that(is.null(ae_time) || is.cs_or_des(ae_time))

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
        list(
          data_extract_input(
            id = ns("mhterm"),
            label = "Select MHTERM variable:",
            data_extract_spec = ui_args$mhterm,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("mhbodsys"),
            label = "Select MHBODSYS variable:",
            data_extract_spec = ui_args$mhbodsys,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Prior medication'"),
        list(
          data_extract_input(
            id = ns("atirel"),
            label = "Select ATIREL:",
            data_extract_spec = ui_args$atirel,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("medname_decoding"),
            label = "Select the medication decoding column:",
            data_extract_spec = ui_args$medname_decoding,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Vitals'"),
        list(
          data_extract_input(
            id = ns("paramcd"),
            label = "Select PARAMCD variable:",
            data_extract_spec = ui_args$paramcd,
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
        list(
          data_extract_input(
            id = ns("ae_term"),
            label = "Select AETERM variable:",
            data_extract_spec = ui_args$ae_term,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_tox_grade"),
            label = "Select AETOXGR variable:",
            data_extract_spec = ui_args$ae_tox_grade,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_causality"),
            label = "Select AEREL variable:",
            data_extract_spec = ui_args$ae_causality,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_outcome"),
            label = "Select AEOUT variable:",
            data_extract_spec = ui_args$ae_outcome,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_action"),
            label = "Select AEACN variable:",
            data_extract_spec = ui_args$ae_action,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_time"),
            label = "Select ASTDY variable:",
            data_extract_spec = ui_args$ae_time,
            is_single_dataset = is_single_dataset_value
          )
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
                                  mhterm,
                                  mhbodsys,
                                  paramcd,
                                  vitals_xaxis,
                                  aval,
                                  atirel,
                                  medname_decoding,
                                  ae_term,
                                  ae_tox_grade,
                                  ae_causality,
                                  ae_outcome,
                                  ae_action,
                                  ae_time,
                                  plot_height,
                                  plot_width,
                                  label) {
  init_chunks()

  # global checks
  validate_checks <- reactive({
    validate(
      need(
        input$`patient_id-dataset_ADSL_singleextract-select`,
        "Please select Patient ID."
      )
    )
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

    validate(
      need(
        input$`binf_vars-dataset_ADSL_singleextract-select`,
        "Please select basic info variables."
      )
    )

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
    data_extract = list(mhterm, mhbodsys),
    input_id = c("mhterm", "mhbodsys"),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  mhist_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`mhterm-dataset_ADMH_singleextract-select`,
        "Please select MHTERM variable."
      ),
      need(
        input$`mhbodsys-dataset_ADMH_singleextract-select`,
        "Please select MHBODSYS variable."
      )
    )

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
      mhterm = input$`mhterm-dataset_ADMH_singleextract-select`,
      mhbodsys = input$`mhbodsys-dataset_ADMH_singleextract-select`
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
    data_extract = list(paramcd, vitals_xaxis, aval, atirel, medname_decoding),
    input_id = c("paramcd", "vitals_xaxis", "aval", "atirel", "medname_decoding"),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  pmed_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`atirel-dataset_ADCM_singleextract-select`,
        "Please select MHBODSYS variable."
      ),
      need(
        input$`medname_decoding-dataset_ADCM_singleextract-select`,
        "Please select Medication decoding variable."
      )
    )

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
      atirel = input$`atirel-dataset_ADCM_singleextract-select`,
      medname_decoding = input$`medname_decoding-dataset_ADCM_singleextract-select`
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
    data_extract = list(paramcd, vitals_xaxis, aval),
    input_id = c("paramcd", "vitals_xaxis", "aval"),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  vitals_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`paramcd-dataset_ADVS_singleextract-select`,
        "Please select PARAMCD variable."
      ),
      need(
        input$`vitals_xaxis-dataset_ADVS_singleextract-select`,
        "Please select Vitals x-axis variable."
      ),
      need(
        input$`aval-dataset_ADVS_singleextract-select`,
        "Please select AVAL variable."
      )
    )

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
    }))

    my_calls <- template_vitals(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      paramcd = input$`paramcd-dataset_ADVS_singleextract-select`,
      vitals_xaxis = input$`vitals_xaxis-dataset_ADVS_singleextract-select`,
      aval = input$`aval-dataset_ADVS_singleextract-select`
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
    data_extract = list(ae_term, ae_tox_grade, ae_causality, ae_outcome, ae_action, ae_time),
    input_id = c("ae_term", "ae_tox_grade", "ae_causality", "ae_outcome", "ae_action", "ae_time"),
    anl_name = "ANL"
  )
  ae_calls <- reactive({
    validate(
      need(
        input$`ae_term-dataset_ADAE_singleextract-select`,
        "Please select AETERM variable."
      ),
      need(
        input$`ae_tox_grade-dataset_ADAE_singleextract-select`,
        "Please select AETOXGR variable."
      ),
      need(
        input$`ae_causality-dataset_ADAE_singleextract-select`,
        "Please select AEREL variable."
      ),
      need(
        input$`ae_outcome-dataset_ADAE_singleextract-select`,
        "Please select AEOUT variable."
      ),
      need(
        input$`ae_action-dataset_ADAE_singleextract-select`,
        "Please select AEACN variable."
      ),
      need(
        input$`ae_time-dataset_ADAE_singleextract-select`,
        "Please select ASTDY variable."
      )
    )

    ae_stack <- chunks$new()
    ae_stack$reset()

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`
    ae_stack$push(bquote({
      ADAE_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    ae_calls <- template_adverse_events(
      ae_term = input$`ae_term-dataset_ADAE_singleextract-select`,
      ae_tox_grade = input$`ae_tox_grade-dataset_ADAE_singleextract-select`,
      ae_causality = input$`ae_causality-dataset_ADAE_singleextract-select`,
      ae_outcome = input$`ae_outcome-dataset_ADAE_singleextract-select`,
      ae_action = input$`ae_action-dataset_ADAE_singleextract-select`,
      ae_time = input$`ae_time-dataset_ADAE_singleextract-select`
    )
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

  # Make sure that get_chunks_object() has the code for the currently viewed tab
  observeEvent(input$tabs, handlerExpr = {
    chunks_reset()
    new_chunks <- switch(input$tabs,
      "Basic info" = basic_info_call(),
      "Medical history" = mhist_call(),
      "Prior medication" = pmed_call(),
      "Vitals" = vitals_call(),
      "Adverse events" = ae_calls())
    if (!is.null(new_chunks)) chunks_push_chunks(new_chunks)
  }, priority = -1)

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(
      patient_id,
      binf_vars,
      mhterm,
      aval,
      paramcd,
      vitals_xaxis,
      ae_term,
      ae_tox_grade,
      ae_causality,
      ae_outcome,
      ae_action,
      ae_time
    )),
    modal_title = label
  )
}
