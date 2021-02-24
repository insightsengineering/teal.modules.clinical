#' Template: Basic Info
#'
#' Creates a basic info template.
#'
#' @inheritParams template_arguments
#' @param patient_id (`character`)\cr Patient ID.
#' @param binf_vars (`character`)\cr variable names to be shown in Basic Info tab.
#' @param mhterm (`character`)\cr name of reported name for medical history variable.
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
#' @param patient_id (`character`)\cr Patient ID.
#' @param mhterm (`character`)\cr name of reported name for medical history variable.
#' @param mhbodsys (`character`)\cr name of body system or organ class variable.
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
#' @param patient_id (`character`)\cr Patient ID.
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param medname_decoding (`character`)\cr name of standardized medication name variable.

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
#' @inheritParams template_arguments
#' @param patient_id (`character`)\cr Patient ID.
#' @param vitals_xaxis (`character`)\cr name of time variable used for the x-axis.
#' @param aval (`character`)\cr name of the analysis value variable.
#' @param vitals_paramcd_levels (`character`)\cr (`paramcd`)\cr vector with (`vitals_paramcd`)\cr levels.
#'
template_vitals <- function(dataname,
                            patient_id,
                            paramcd = "PARAMCD",
                            vitals_paramcd_levels = c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP"),
                            vitals_xaxis = "ADY",
                            aval = "AVAL") {
  assert_that(
    is.string(dataname),
    is.string(paramcd),
    is.string(vitals_xaxis),
    is.string(aval)
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
        filter(paramcd %in% vitals_paramcd_levels_chars) %>%
        summarise(
          AVAL = max(aval, na.rm = T)
        )

      max_day <- max(vitals[[vitals_xaxis_char]], na.rm = T)
      max_aval <- max(vitals[[aval_char]], na.rm = T)
      max_aval_seq <- seq(0, max_aval, 10)

      provided_vita <- vitals_paramcd_levels_chars
      len_paramcd_levels_chars <- length(provided_vita)
      known_vita <- c("SYSBP", "DIABP", "TEMP", "RESP", "OXYSAT", "PULSE")

      vitals_paramcd_levels_e <- known_vita[na.omit(pmatch(provided_vita, known_vita))]
      len_vitals_paramcd_levels_e <- length(vitals_paramcd_levels_e)

      vars_colors <- color_palette(len_paramcd_levels_chars)
      names(vars_colors) <- provided_vita

      base_stats <- setNames(c(140, 90, 38, 20, 94, 100), known_vita)
      paramcd_stats_e <- base_stats[vitals_paramcd_levels_e]

      base_labels <- setNames(c("140mmHg", "90mmHg", "38\u00B0 C", "20/min", "94%", "100bpm"), known_vita)
      paramcd_labels_e <- base_labels[vitals_paramcd_levels_e]

      base_stats_df <- data.frame(
        x = rep(1, len_vitals_paramcd_levels_e),
        y = paramcd_stats_e,
        label = paramcd_labels_e,
        color = vitals_paramcd_levels_e
      )

      result_plot <- ggplot(data = vitals, mapping = aes(x = vitals_xaxis)) + # replaced VSDY
        geom_line(
          data = vitals,
          mapping = aes(y = aval, color = paramcd),
          size = 1.5,
          alpha = 0.5
        ) +
        scale_color_manual(
          values = vars_colors, # removed  "PUL", "OXYSAT", "WGHT"
        ) +
        geom_text(
          data = base_stats_df,
          aes(x = x, y = y, label = label, color = color), alpha = 1, nudge_y = 2.2
        ) +
        geom_hline(
          data = base_stats_df,
          aes(yintercept = y, color = color), linetype = 2, alpha = 0.5, size = 1
        ) +
        scale_x_continuous(
          limits = c(1, max_day)
        ) +
        scale_y_continuous(
          breaks = seq(0, max(vitals[[vitals_xaxis_char]], na.rm = T), 50),
          name = "Vitals",
          minor_breaks = seq(0, max(vitals[[aval_char]], na.rm = T), 10)
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
        theme_minimal() +
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
      vitals_paramcd_levels_chars = vitals_paramcd_levels,
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
#' @param patient_id (`character`)\cr Patient ID.
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param medname_decoding (`character`)\cr name of standardized medication name variable.
#' @param cmindc (`character`)\cr name of indication variable.
#' @param cmdose (`character`)\cr name of dose per administration variable.
#' @param cmtrt (`character`)\cr name of reported name of drug, med, or therapy variable.
#' @param cmdosu (`character`)\cr name of dose units variable.
#' @param cmroute (`character`)\cr name of route of administration variable.
#' @param cmdosfrq (`character`)\cr name of dosing frequency per interval variable.
#' @param cmstdy (`character`)\cr name of study day of start of medication variable.
#' @param cmendy (`character`)\cr name of study day of end of medication variable.

template_therapy <- function(dataname,
                             patient_id,
                             atirel = "ATIREL",
                             medname_decoding = "CMDECOD",
                             cmindc = "CMINDC",
                             cmdose = "CMDOSE",
                             cmtrt = "CMTRT",
                             cmdosu = "CMDOSU",
                             cmroute = "CMROUTE",
                             cmdosfrq = "CMDOSFRQ",
                             cmstdy = "CMSTDY", # replaces CMSTDY
                             cmendy = "CMENDY" # replaces CMENDY
) {
  assert_that(
    is.string(dataname),
    is.string(atirel),
    is.string(medname_decoding),
    is.string(cmindc),
    is.string(cmdose),
    is.string(cmtrt),
    is.string(cmdosu),
    is.string(cmroute),
    is.string(cmdosfrq),
    is.string(cmstdy),
    is.string(cmendy)
  )

  y <- list()
  y$table_list <- list()
  y$plot_list <- list()

  #
  table_list <- add_expr(
    list(),
    substitute(expr = {
      cols_to_inlude <- c(
        cmindc_char,
        cmdecod_char,
        cmdose_char,
        cmtrt_char,
        cmdosu_char,
        cmroute_char,
        cmdosfrq_char,
        cmstdy_char,
        cmendy_char
      )

      for (col in cols_to_inlude) {
        if (!(col %in% names(dataname))) {
          dataname[[col]] <- NA
        }
      }

      therapy_table <-
        dataname %>%
        filter(atirel %in% c("CONCOMITANT", "PRIOR")) %>% # removed PRIOR_CONCOMITANT
        select(cols_to_inlude) %>%
        filter(!is.na(cmdecod)) %>%
        mutate(Dosage = paste(cmdose, cmdosu, cmdosfrq, cmroute)) %>%
        select(-cmdose, -cmdosu, -cmdosfrq, -cmroute) %>%
        select(cmindc, cmdecod, Dosage, everything()) %>%
        mutate(CMDECOD = case_when(
          nchar(as.character(cmdecod)) > 20 ~ as.character(cmtrt),
          TRUE ~ as.character(cmdecod)
        )) %>%
        select(-cmtrt) %>%
        arrange(cmindc, cmdecod, cmstdy) %>%
        distinct()
      # `colnames<-`(get_labels(dataname)$column_labels[cmindc, cmdecod, cmstdy]) # nolintr
    }, env = list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(medname_decoding),
      cmindc = as.name(cmindc),
      cmdose = as.name(cmdose),
      cmtrt = as.name(cmtrt),
      cmdosu = as.name(cmdosu),
      cmroute = as.name(cmroute),
      cmdosfrq = as.name(cmdosfrq),
      cmstdy = as.name(cmstdy),
      cmendy = as.name(cmendy),
      cmdecod_char = medname_decoding,
      cmindc_char = cmindc,
      cmdose_char = cmdose,
      cmtrt_char = cmtrt,
      cmdosu_char = cmdosu,
      cmroute_char = cmroute,
      cmdosfrq_char = cmdosfrq,
      cmendy_char = cmendy,
      cmstdy_char = cmstdy
    ))
  )

  plot_list <- add_expr(
    list(),
    substitute(expr = {
      max_day <- max(dataname[[cmendy_char]], na.rm = T)
      dataname %>%
        filter(atirel %in% c("CONCOMITANT", "PRIOR")) %>% # remove PRIOR_CONCOMITANT
        select_at(cols_to_inlude) %>%
        filter(!is.na(cmdecod)) %>%
        mutate(DOSE = paste(cmdose, cmdosu, cmdosfrq)) %>%
        select(-cmdose, -cmdosu, -cmdosfrq) %>%
        select(cmindc, cmdecod, DOSE, everything()) %>%
        arrange(cmindc, cmdecod, cmstdy) %>%
        distinct() %>%
        mutate(CMSTDY = case_when(
          is.na(cmstdy) ~ 1,
          cmstdy < 1 ~ 1,
          TRUE ~ cmstdy
        )) %>%
        mutate(CMENDY = case_when(
          is.na(cmendy) ~ max_day,
          TRUE ~ cmendy
        )) %>%
        arrange(CMSTDY, desc(CMSTDY)) %>%
        mutate(CMDECOD = case_when(
          nchar(as.character(cmdecod)) > 20 ~ as.character(cmtrt),
          TRUE ~ as.character(cmdecod)
        )) ->
      data

      therapy_plot <-
        ggplot(data = data, aes(fill = cmindc, color = cmindc, y = CMDECOD, x = CMSTDY)) +
        geom_segment(aes(xend = CMENDY, yend = CMDECOD), size = 2) +
        geom_text(
          data =
            data %>%
              select(CMDECOD, cmindc) %>%
            distinct(),
          aes(x = 1, label = CMDECOD), color = "black",
          hjust = "left",
          vjust = "bottom",
          nudge_y = 0.1,
          size = 3
        ) +
        xlim(1, max_day) +
        scale_y_discrete(expand = expansion(add = 1.2)) +
        geom_point(color = "black", size = 2, shape = 24, position = position_nudge(y = -0.15)) +
        theme_minimal() +
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
        ylab("Medication") +
        theme(legend.position = "none")
    }, env = list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(medname_decoding),
      cmindc = as.name(cmindc),
      cmdose = as.name(cmdose),
      cmtrt = as.name(cmtrt),
      cmdosu = as.name(cmdosu),
      cmroute = as.name(cmroute),
      cmdosfrq = as.name(cmdosfrq),
      cmstdy = as.name(cmstdy),
      cmendy = as.name(cmendy),
      cmdecod_char = medname_decoding,
      cmindc_char = cmindc,
      cmdose_char = cmdose,
      cmtrt_char = cmtrt,
      cmdosu_char = cmdosu,
      cmroute_char = cmroute,
      cmdosfrq_char = cmdosfrq,
      cmstdy_char = cmstdy,
      cmendy_char = cmendy
    ))
  )
  y$table_list <- bracket_expr(table_list)
  y$plot_list <- bracket_expr(plot_list)
  y
}


#' Template: Adverse Events Tab
#'
#' Creates an adverse events template call.
#'
#' @inheritParams template_arguments
#' @param patient_id (`character`)\cr Patient ID.
#' @param ae_term (`character`)\cr name of the reported term for the adverse event variable.
#' @param ae_tox_grade (`character`)\cr name of the standard toxicity grade variable.
#' @param ae_causality (`character`)\cr name of the causality variable.
#' @param ae_outcome (`character`)\cr name of outcome of adverse event variable.
#' @param ae_action (`character`)\cr name of action taken with study treatment variable.
#' @param ae_time (`character`)\cr name of study day of start of adverse event variable.
#'
template_adverse_events <- function(patient_id,
                                    dataname = "ADAE_FILTERED",
                                    ae_term = "AETERM",
                                    ae_tox_grade = "AETOXGR",
                                    ae_causality = "AEREL",
                                    ae_outcome = "AEOUT",
                                    ae_action = "AEACN",
                                    ae_time = "ASTDY",
                                    ae_decod = NULL) {
  assert_that(
    is.string(dataname),
    is.string(ae_term),
    is.string(ae_tox_grade),
    is.string(ae_causality),
    is.string(ae_outcome),
    is.string(ae_action),
    is.string(ae_time) || is.null(ae_time),
    is.string(ae_decod) || is.null(ae_decod)
  )

  y <- list()

  y$table <- list()
  y$chart <- list()

  table_list <- add_expr(
    list(),
    substitute(
      ae_table <- dataname %>%
        select(
          ae_term, ae_tox_grade, ae_causality, ae_outcome, ae_action, ae_time, ae_decod
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
        ae_decod = if_not_null(ae_decod, as.name(ae_decod)),
        ae_vars = c(ae_term, ae_tox_grade, ae_causality, ae_outcome, ae_action, ae_time, ae_decod)
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
          scale_fill_manual(values = c(
            "1" = "#E2264633",
            "2" = "#E2264666",
            "3" = "#E2264699",
            "4" = "#E22646CC",
            "5" = "#E22646FF",
            "UNKNOWN" = "#ACADB1FF"
            )) +
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

#' Template: Patient Timeline Tab
#'
#' Creates a patient timeline template call.
#'
#' @inheritParams template_arguments
#' @param patient_id (`character`)\cr Patient ID.
#' @param ae_term (`character`)\cr name of the reported term for the adverse event variable.
#' @param ae_time_start (`character`)\cr name of datetime start of adverse event variable.
#' @param ae_time_end (`character`)\cr name of datetime end of adverse event variable.
#' @param ds_time_start (`character`)\cr name of datetime first exposure to treatment variable.
#' @param ds_time_end (`character`)\cr name of datetime last exposure to treatment variable.
#' @param cmtrt (`character`)\cr name of reported name of drug, med, or therapy variable.
#' @importFrom timevis timevis
#'
template_patient_timeline <- function(patient_id,
                                      dataname = "ANL_FILTERED",
                                      ae_term = "AETERM",
                                      ae_time_start = "ASTDTM", # to be updated
                                      ae_time_end = "AENDTM", # to be updated
                                      ds_time_start = "CMASTDTM", # to be updated
                                      ds_time_end = "CMAENDTM", # to be updated
                                      cmtrt = "CMTRT") {
  # Note: The variables used for ae_time_start, ae_time_end, ds_time_start and ds_time_end are to be updated after
  # random.cdisc.data updates.
  assert_that(
    is.string(dataname)
  )

  y <- list()
  y$chart <- list()

  chart_list <- add_expr(
    list(),
    substitute(
      expr = {
        # three sections are represented here: advers events, dosing and medication
        ae_chart <- dataname %>%
          select(ae_term, ae_time_start, ae_time_end) %>%
          distinct()

        ds_chart <- dataname %>%
          select(ds_time_start, ds_time_end) %>%
          distinct() %>%
          mutate(
            label_start = "First Exposure to Treatment",
            label_end = "Last Exposure to Treatment"
          )

        med_chart <- dataname %>%
          select(cmtrt, ds_time_start, ds_time_end) %>%
          distinct()

        min_ds_chart_time_start <- min(ds_chart[[ds_time_start_var]])
        min_ds_chart_time_end <- min(ds_chart[[ds_time_end_var]])
        max_ds_chart_time_end <- max(ds_chart[[ds_time_end_var]])

        timevis_data <- data.frame(
          id = seq_len(nrow(ae_chart) +
            length(unique(ds_chart[["label_start"]])) +
            length(unique(ds_chart[["label_end"]])) +
            nrow(med_chart)),
          content = c(
            as.character(ae_chart[[ae_term_var]]),
            as.character(unique(ds_chart[["label_start"]])),
            as.character(unique(ds_chart[["label_end"]])),
            as.character(med_chart[[cmtrt_var]])
          ),
          start = c(
            ae_chart[[ae_time_start_var]],
            min_ds_chart_time_start,
            max_ds_chart_time_end,
            med_chart[[ds_time_start_var]]
          ),
          end = c(
            ae_chart[[ae_time_end_var]],
            min_ds_chart_time_start,
            max_ds_chart_time_end,
            med_chart[[ds_time_end_var]]
          ),
          group = c(
            rep("AE", length(ae_chart[[ae_term_var]])),
            rep("DOS", length(min_ds_chart_time_start)),
            rep("DOS", length(min_ds_chart_time_end)),
            rep("MED", length(med_chart[[cmtrt_var]]))
          ),
          type = c(
            rep("range", length(ae_chart[[ae_term_var]])),
            rep("point", length(min_ds_chart_time_start)),
            rep("point", length(min_ds_chart_time_end)),
            rep("range", length(med_chart[[cmtrt_var]]))
          )
        )

        timevis_data_groups <- data.frame(
          id = c("AE", "DOS", "MED"),
          content = c(
            "Adverse Events", "Dosing", "Medication"
          )
        )

        patient_timeline_plot <-
          timevis::timevis(data = timevis_data, groups = timevis_data_groups)
      },
      env = list(
        dataname = as.name(dataname),
        ae_term = as.name(ae_term),
        ae_time_start = as.name(ae_time_start),
        ae_time_end = as.name(ae_time_end),
        ds_time_start = as.name(ds_time_start),
        ds_time_end = as.name(ds_time_end),
        cmtrt = as.name(cmtrt),
        ae_term_var = ae_term,
        ae_time_start_var = ae_time_start,
        ae_time_end_var = ae_time_end,
        ds_time_start_var = ds_time_start,
        ds_time_end_var = ds_time_end,
        cmtrt_var = cmtrt
      )
    )
  )

  chart_list <- add_expr(
    expr_ls = chart_list,
    new_expr = quote(patient_timeline_plot)
  )

  y$chart <- bracket_expr(chart_list)
  y
}


#' Template: Laboratory
#'
#' Creates a latoratory template.
#' @inheritParams template_arguments
#' @param patient_id (`character`)\cr Patient ID.
#' @param paramcd (`choices selected` or `data_extract_input`)\cr \code{PARAMCD} column of the ADLB dataset.
#' @param param (`choices selected` or `data_extract_input`)\cr \code{PARAM} column of the ADLB dataset.
#' @param labor_timepoints  (`choices selected` or `data_extract_input`)\cr name of time variable used for
#' the laboratory table.
#' @param anrind (`choices selected` or `data_extract_input`)\cr \code{ANRIND} column of the ADLB dataset
#' with 3 possible levels "HIGH", "LOW" and "NORMAL".
#' @param aval (`choices selected` or `data_extract_input`)\cr \code{AVAL} column of the ADLB dataset.
#' @param avalu (`choices selected` or `data_extract_input`)\cr \code{AVALU} column of the ADLB dataset.
#' @importFrom tidyr pivot_wider
#'
template_laboratory <- function(patient_id,
                                dataname = "ADLB_FILTERED",
                                paramcd = "PARAMCD",
                                param = "PARAM",
                                anrind = "ANRIND",
                                labor_timepoints = "ADY",
                                aval = "AVAL",
                                avalu = "AVALU") {
  assert_that(
    is.string(dataname),
    is.string(paramcd),
    is.string(param),
    is.string(anrind),
    is.string(labor_timepoints),
    is.string(aval),
    is.string(avalu)
  )

  y <- list()
  y$table <- list()

  table_lab_list <- add_expr(
    list(),
    substitute({
      labor_table <- dataname %>%
        select(labor_timepoints, paramcd, param, aval, avalu, anrind) %>%
        arrange(labor_timepoints) %>%
        select(-labor_timepoints) %>%
        mutate(aval_anrind = color_lab_values(paste(aval, anrind))) %>%
        select(-c(aval, anrind)) %>%
        group_by(paramcd, param) %>%
        mutate(INDEX = row_number()) %>%
        ungroup() %>%
        pivot_wider(names_from = INDEX, values_from = aval_anrind) %>%
        mutate(PARAM = stringr::str_replace_all(param, "\\(.*?\\)", "")) %>%
        mutate(PARAM = stringr::str_squish(param)) %>%
        mutate(PARAM = stringr::str_trunc(param, width = 20))
      },
      env = list(
        dataname = as.name(dataname),
        param = as.name(param),
        paramcd = as.name(paramcd),
        aval = as.name(aval),
        avalu = as.name(avalu),
        labor_timepoints = as.name(labor_timepoints),
        anrind = as.name(anrind)
      )
    )
  )

  y$table <- bracket_expr(table_lab_list)
  y
}


#' Teal Module: Patient Profile Teal Module
#'
#' This teal module produces a patient profile report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_id (`choices selected` or `data_extract_input`)\cr patient ID column to be used.
#' @param binf_vars (`choices selected` or `data_extract_input`)\cr ADSL columns to be shown in Basic Info tab.
#' @param ae_term (`choices selected` or `data_extract_input`)\cr \code{AETERM} column of the ADAE dataset.
#' @param ae_tox_grade (`choices selected` or `data_extract_input`)\cr \code{AETOXGR} column of the ADAE dataset.
#' @param ae_causality (`choices selected` or `data_extract_input`)\cr \code{AEREL} column of the ADAE dataset.
#' @param ae_outcome (`choices selected` or `data_extract_input`)\cr \code{AEOUT} column of the ADAE dataset.
#' @param ae_action (`choices selected` or `data_extract_input`)\cr \code{AEACN} column of the ADAE dataset.
#' @param ae_time (`choices selected` or `data_extract_input`)\cr \code{ASTDY} column of the ADAE dataset.
#' @param ae_decod (`choices selected` or `data_extract_input`)\cr \code{AEDECOD} column of the ADAE dataset.
#' @param mhterm (`choices selected` or `data_extract_input`)\cr \code{MHTERM} column of the ADMH dataset.
#' @param mhbodsys (`choices selected` or `data_extract_input`)\cr \code{MHBODSYS} column of the ADMH dataset.
#' @param vitals_paramcd (`choices selected` or `data_extract_input`)\cr \code{PARAMCD} column of the ADVS dataset.
#' @param vitals_param (`choices selected` or `data_extract_input`)\cr \code{PARAM} column of the ADVS dataset.
#' @param labor_paramcd (`choices selected` or `data_extract_input`)\cr \code{PARAMCD} column of the ADLB dataset.
#' @param labor_param (`choices selected` or `data_extract_input`)\cr \code{PARAM} column of the ADLB dataset.
#' @param vitals_xaxis (`choices selected` or `data_extract_input`)\cr Time variable to be represented in
#' the vitals plot x-axis.
#' @param labor_timepoints (`choices selected` or `data_extract_input`)\cr Time variable to be represented in
#' the laboratory table.
#' @param anrind (`choices selected` or `data_extract_input`)\cr \code{ANRIND} column of the ADLB dataset
#' with 3 possible levels "HIGH", "LOW" and "NORMAL".
#' @param vitals_aval (`choices selected` or `data_extract_input`)\cr \code{AVAL} column of the ADVS dataset.
#' @param labor_aval (`choices selected` or `data_extract_input`)\cr \code{AVAL} column of the ADLB dataset.
#' @param labor_avalu (`choices selected` or `data_extract_input`)\cr \code{AVALU} column of the ADLB dataset.
#' @param atirel (`choices selected` or `data_extract_input`)\cr \code{ATIREL} column of the ADCM dataset.
#' @param medname_decoding (`choices selected` or `data_extract_input`)\cr \code{CMDECOD} column of the ADCM dataset.
#' @param cmindc (`choices selected` or `data_extract_input`)\cr \code{CMINDC} column of the ADCM dataset.
#' @param cmdose (`choices selected` or `data_extract_input`)\cr \code{CMDOSE} column of the ADCM dataset.
#' @param cmtrt (`choices selected` or `data_extract_input`)\cr \code{CMTRT} column of the ADCM dataset.
#' @param cmdosu (`choices selected` or `data_extract_input`)\cr \code{CMDOSU} column of the ADCM dataset.
#' @param cmroute (`choices selected` or `data_extract_input`)\cr \code{CMROUTE} column of the ADCM dataset.
#' @param cmdosfrq (`choices selected` or `data_extract_input`)\cr \code{CMDOSFRQ} column of the ADCM dataset.
#' @param cmstdy (`choices selected` or `data_extract_input`)\cr \code{CMSTDY} column of the ADCM dataset.
#' @param cmendy (`choices selected` or `data_extract_input`)\cr \code{CMENDY} column of the ADCM dataset.
#' @param ae_time_start (`choices selected` or `data_extract_input`)\cr
#' \code{CMENDY} column of the AE start of the ADCM dataset.
#' @param  ae_time_end (`choices selected` or `data_extract_input`)\cr
#' \code{CMENDY} column of the AE end of the ADCM dataset.
#' @param ds_time_start (`choices selected` or `data_extract_input`)\cr
#' \code{CMENDY} column of treatment start of the ADCM dataset.
#' @param ds_time_end (`choices selected` or `data_extract_input`)\cr
#' \code{CMENDY} column of treatment end of the ADCM dataset.
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
#' ADLB <- radlb(cached = TRUE)
#'
#' #' Modify ADCM
#' ADCM$CMINDC <- ADCM$CMCAT
#' ADCM$CMDECOD <- ADCM$CMCAT
#' ADCM$CMDOSE <- 1
#' ADCM$CMTRT <- ADCM$CMCAT
#' ADCM$CMDOSU <- "U"
#' ADCM$CMROUTE <- "CMROUTE"
#' ADCM$CMDOSFRQ <- "CMDOSFRQ"
#' ADCM$CMSTDY <- 1
#' ADCM[ADCM$CMCAT == "medcl B", ]$CMSTDY <- 20
#' ADCM[ADCM$CMCAT == "medcl C", ]$CMSTDY <- 150
#' ADCM$CMENDY <- 500
#' ADCM[ADCM$CMCAT == "medcl B", ]$CMENDY <- 700
#' ADCM[ADCM$CMCAT == "medcl C", ]$CMENDY <- 1000
#' ADCM$CMASTDTM <- ADCM$ASTDTM
#' ADCM$CMAENDTM <- ADCM$AENDTM
#'
#' ids <- unique(ADSL$USUBJID)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- radae(cached = TRUE)"),
#'     cdisc_dataset("ADMH", ADMH, code = "ADMH <- radmh(cached = TRUE)"),
#'     cdisc_dataset("ADCM", ADCM, code = "ADCM <- radcm(cached = TRUE)
#'                   ADCM$CMINDC <- ADCM$CMCAT
#'                   ADCM$CMDECOD <- ADCM$CMCAT
#'                   ADCM$CMDOSE <- 1
#'                   ADCM$CMTRT <- ADCM$CMCAT
#'                   ADCM$CMDOSU <- 'U'
#'                   ADCM$CMROUTE <- 'CMROUTE'
#'                   ADCM$CMDOSFRQ <- 'CMDOSFRQ'
#'                   ADCM$CMSTDY <- 1
#'                   ADCM[ADCM$CMCAT == 'medcl B' ,]$CMSTDY <- 20
#'                   ADCM[ADCM$CMCAT == 'medcl C' ,]$CMSTDY <- 150
#'                   ADCM$CMENDY <- 500
#'                   ADCM[ADCM$CMCAT == 'medcl B' ,]$CMENDY <- 700
#'                   ADCM[ADCM$CMCAT == 'medcl C' ,]$CMENDY <- 1000
#'                   ADCM$CMASTDTM <- ADCM$ASTDTM
#'                   ADCM$CMAENDTM <- ADCM$AENDTM
#'                   "),
#'     cdisc_dataset("ADVS", ADVS, code = "ADVS <- radvs(cached = TRUE)"),
#'     cdisc_dataset("ADLB", ADLB, code = "ADLB <- radlb(cached = TRUE)"),
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
#'           choices = variable_choices(ADMH, c("MHTERM")),
#'           selected = c("MHTERM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       mhbodsys = data_extract_spec(
#'         dataname = "ADMH",
#'         select = select_spec(
#'           choices = variable_choices(ADMH, "MHBODSYS"),
#'           selected = c("MHBODSYS"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       vitals_paramcd = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "PARAMCD"),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       vitals_param = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "PARAM"),
#'           selected = c("PARAM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       labor_paramcd = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "PARAMCD"),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       labor_param = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "PARAM"),
#'           selected = c("PARAM"),
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
#'       labor_timepoints = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "ADY"),
#'           selected = c("ADY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       anrind = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "ANRIND"),
#'           selected = c("ANRIND"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       vitals_aval = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "AVAL"),
#'           selected = c("AVAL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       labor_aval = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "AVAL"),
#'           selected = c("AVAL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       labor_avalu = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "AVALU"),
#'           selected = c("AVALU"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       atirel = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "ATIREL"),
#'           selected = c("ATIREL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       medname_decoding = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDECOD"),
#'           selected = c("CMDECOD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmindc = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMINDC"),
#'           selected = c("CMINDC"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmdose = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDOSE"),
#'           selected = c("CMDOSE"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmtrt = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMTRT"),
#'           selected = c("CMTRT"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmdosu = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDOSU"),
#'           selected = c("CMDOSU"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmroute = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMROUTE"),
#'           selected = c("CMROUTE"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmdosfrq = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDOSFRQ"),
#'           selected = c("CMDOSFRQ"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmstdy = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMSTDY"),
#'           selected = c("CMSTDY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       cmendy = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMENDY"),
#'           selected = c("CMENDY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_term = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AETERM"),
#'           selected = c("AETERM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_tox_grade = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AETOXGR"),
#'           selected = c("AETOXGR"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_causality = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AEREL"),
#'           selected = c("AEREL"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_outcome = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AEOUT"),
#'           selected = c("AEOUT"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_action = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AEACN"),
#'           selected = c("AEACN"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_time = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "ASTDY"),
#'           selected = c("ASTDY"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_decod = NULL,
#'       ae_time_start = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "ASTDTM"),
#'           selected = c("ASTDTM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_time_end = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AENDTM"),
#'           selected = c("AENDTM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ds_time_start = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMASTDTM"),
#'           selected = c("CMASTDTM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ds_time_end = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMAENDTM"),
#'           selected = c("CMAENDTM"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' #' Not run:
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
                                 vitals_paramcd = NULL,
                                 vitals_param = NULL,
                                 labor_paramcd = NULL,
                                 labor_param = NULL,
                                 vitals_xaxis = NULL,
                                 labor_timepoints = NULL,
                                 anrind = NULL,
                                 vitals_aval = NULL,
                                 labor_aval = NULL,
                                 labor_avalu = NULL,
                                 ae_term = NULL,
                                 ae_tox_grade = NULL,
                                 ae_causality = NULL,
                                 ae_outcome = NULL,
                                 ae_action = NULL,
                                 ae_time = NULL,
                                 ae_time_start = NULL,
                                 ae_time_end = NULL,
                                 ae_decod = NULL,
                                 cmindc = NULL,
                                 cmdose = NULL,
                                 cmtrt = NULL,
                                 cmdosu = NULL,
                                 cmroute = NULL,
                                 cmdosfrq = NULL,
                                 cmstdy = NULL,
                                 cmendy = NULL,
                                 ds_time_start = NULL,
                                 ds_time_end = NULL,
                                 plot_height = c(700L, 200L, 2000L),
                                 plot_width = c(900L, 200L, 2000L),
                                 pre_output = NULL,
                                 post_output = NULL) {
  assert_that(is_character_single(label))
  assert_that(is_character_single(dataname))
  assert_that(is_character_single(parentname))
  assert_that(is.null(pre_output) || is(pre_output, "shiny.tag"),
    msg = "pre_output should be either null or shiny.tag type of object"
  )
  assert_that(is.null(post_output) || is(post_output, "shiny.tag"),
    msg = "post_output should be either null or shiny.tag type of object"
  )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  data_extract_list <- list(
    patient_id = cs_to_des_select(patient_id, dataname = parentname),
    binf_vars = if_not_null(binf_vars, cs_to_des_select(binf_vars, dataname = parentname)),
    mhterm = if_not_null(mhterm, cs_to_des_select(mhterm, dataname = parentname)),
    mhbodsys = if_not_null(mhbodsys, cs_to_des_select(mhbodsys, dataname = parentname)),
    vitals_paramcd = if_not_null(vitals_paramcd, cs_to_des_select(vitals_paramcd, dataname = parentname)),
    vitals_param = if_not_null(vitals_param, cs_to_des_select(vitals_param, dataname = parentname)),
    labor_paramcd = if_not_null(labor_paramcd, cs_to_des_select(labor_paramcd, dataname = parentname)),
    labor_param = if_not_null(labor_param, cs_to_des_select(labor_param, dataname = parentname)),
    vitals_xaxis = if_not_null(vitals_xaxis, cs_to_des_select(vitals_xaxis, dataname = parentname)),
    labor_timepoints = if_not_null(labor_timepoints, cs_to_des_select(labor_timepoints, dataname = parentname)),
    anrind = if_not_null(anrind, cs_to_des_select(anrind, dataname = parentname)),
    vitals_aval = if_not_null(vitals_aval, cs_to_des_select(vitals_aval, dataname = parentname)),
    labor_aval = if_not_null(labor_aval, cs_to_des_select(labor_aval, dataname = parentname)),
    labor_avalu = if_not_null(labor_avalu, cs_to_des_select(labor_avalu, dataname = parentname)),
    atirel = if_not_null(atirel, cs_to_des_select(atirel, dataname = parentname)),
    medname_decoding = if_not_null(medname_decoding, cs_to_des_select(medname_decoding, dataname = parentname)),
    ae_term = if_not_null(ae_term, cs_to_des_select(ae_term, dataname = parentname)),
    ae_tox_grade = if_not_null(ae_tox_grade, cs_to_des_select(ae_tox_grade, dataname = parentname)),
    ae_causality = if_not_null(ae_causality, cs_to_des_select(ae_causality, dataname = parentname)),
    ae_outcome = if_not_null(ae_outcome, cs_to_des_select(ae_outcome, dataname = parentname)),
    ae_action = if_not_null(ae_action, cs_to_des_select(ae_action, dataname = parentname)),
    ae_time = if_not_null(ae_time, cs_to_des_select(ae_time, dataname = parentname)),
    ae_time_start = if_not_null(ae_time_start, cs_to_des_select(ae_time_start, dataname = parentname)),
    ae_time_end = if_not_null(ae_time_end, cs_to_des_select(ae_time_end, dataname = parentname)),
    ae_decod = if_not_null(ae_decod, cs_to_des_select(ae_decod, dataname = parentname)),
    cmindc = if_not_null(cmindc, cs_to_des_select(cmindc, dataname = parentname)),
    cmdose = if_not_null(cmdose, cs_to_des_select(cmdose, dataname = parentname)),
    cmtrt = if_not_null(cmtrt, cs_to_des_select(cmtrt, dataname = parentname)),
    cmdosu = if_not_null(cmdosu, cs_to_des_select(cmdosu, dataname = parentname)),
    cmdosfrq = if_not_null(cmdosfrq, cs_to_des_select(cmdosfrq, dataname = parentname)),
    cmroute = if_not_null(cmroute, cs_to_des_select(cmroute, dataname = parentname)),
    cmstdy = if_not_null(cmstdy, cs_to_des_select(cmstdy, dataname = parentname)),
    cmendy = if_not_null(cmendy, cs_to_des_select(cmendy, dataname = parentname)),
    ds_time_start = if_not_null(ds_time_start, cs_to_des_select(ds_time_start, dataname = parentname)),
    ds_time_end = if_not_null(ds_time_end, cs_to_des_select(ds_time_end, dataname = parentname))
  )
  assert_that(is.cs_or_des(patient_id))
  assert_that(is.null(binf_vars) || is.cs_or_des(binf_vars))
  assert_that(is.null(mhterm) || is.cs_or_des(mhterm))
  assert_that(is.null(mhbodsys) || is.cs_or_des(mhbodsys))
  assert_that(is.null(vitals_paramcd) || is.cs_or_des(vitals_paramcd))
  assert_that(is.null(labor_paramcd) || is.cs_or_des(labor_paramcd))
  assert_that(is.null(vitals_xaxis) || is.cs_or_des(vitals_xaxis))
  assert_that(is.null(vitals_aval) || is.cs_or_des(vitals_aval))
  assert_that(is.null(labor_aval) || is.cs_or_des(labor_aval))
  assert_that(is.null(atirel) || is.cs_or_des(atirel))
  assert_that(is.null(medname_decoding) || is.cs_or_des(medname_decoding))
  assert_that(is.null(ae_term) || is.cs_or_des(ae_term))
  assert_that(is.null(ae_tox_grade) || is.cs_or_des(ae_tox_grade))
  assert_that(is.null(ae_causality) || is.cs_or_des(ae_causality))
  assert_that(is.null(ae_outcome) || is.cs_or_des(ae_outcome))
  assert_that(is.null(ae_action) || is.cs_or_des(ae_action))
  assert_that(is.null(ae_time) || is.cs_or_des(ae_time))
  assert_that(is.null(ae_time_end) || is.cs_or_des(ae_time_end))
  assert_that(is.null(cmindc) || is.cs_or_des(cmindc))
  assert_that(is.null(cmdose) || is.cs_or_des(cmdose))
  assert_that(is.null(cmtrt) || is.cs_or_des(cmtrt))
  assert_that(is.null(cmdosu) || is.cs_or_des(cmdosu))
  assert_that(is.null(cmdosfrq) || is.cs_or_des(cmdosfrq))
  assert_that(is.null(cmroute) || is.cs_or_des(cmroute))
  assert_that(is.null(cmstdy) || is.cs_or_des(cmstdy))
  assert_that(is.null(cmendy) || is.cs_or_des(cmendy))
  assert_that(is.null(ds_time_start) || is.cs_or_des(ds_time_start))
  assert_that(is.null(ds_time_end) || is.cs_or_des(ds_time_end))

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

#' @importFrom timevis timevisOutput
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
            DT::DTOutput(outputId = ns("therapy_table")),
            plot_with_settings_ui(id = ns("therapy_plot"))
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
            DT::DTOutput(outputId = ns("lab_values"))
          )
        ),
        tabPanel(
          "Patient timeline",
          div(
            timevis::timevisOutput(outputId = ns("patient_timeline_plot"))
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
            id = ns("vitals_paramcd"),
            label = "Select PARAMCD variable:",
            data_extract_spec = ui_args$vitals_paramcd,
            is_single_dataset = is_single_dataset_value
          ),
          uiOutput(ns("vitals_paramcd_levels")),
          data_extract_input(
            id = ns("vitals_xaxis"),
            label = "Select vital plot x-axis:",
            data_extract_spec = ui_args$vitals_xaxis,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("vitals_aval"),
            label = "Select AVAL variable:",
            data_extract_spec = ui_args$vitals_aval,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Therapy'"),
        list(
          data_extract_input(
            id = ns("atirel"),
            label = "Select ATIREL variable:",
            data_extract_spec = ui_args$atirel,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("medname_decoding"),
            label = "Select medication decoding column:",
            data_extract_spec = ui_args$medname_decoding,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmindc"),
            label = "Select CMINDC variable:",
            data_extract_spec = ui_args$cmindc,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmdose"),
            label = "Select CMDOSE variable:",
            data_extract_spec = ui_args$cmdose,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmtrt"),
            label = "Select CMTRT variable:",
            data_extract_spec = ui_args$cmtrt,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmdosu"),
            label = "Select CMDOSU variable:",
            data_extract_spec = ui_args$cmdosu,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmroute"),
            label = "Select CMROUTE variable:",
            data_extract_spec = ui_args$cmroute,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmdosfrq"),
            label = "Select CMDOSFRQ variable:",
            data_extract_spec = ui_args$cmdosfrq,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmstdy"),
            label = "Select CMSTDY variable:",
            data_extract_spec = ui_args$cmstdy,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmendy"),
            label = "Select CMENDY variable:",
            data_extract_spec = ui_args$cmendy,
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
          ),
          if_not_null(
            ui_args$ae_decod,
            data_extract_input(
              id = ns("ae_decod"),
              label = "Select DECOD variable:",
              data_extract_spec = ui_args$ae_decod,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      conditionalPanel(
        condition =
          paste0("input['", ns("tabs"), "'] == 'Patient timeline'"),
        list(
          data_extract_input(
            id = ns("ae_term"),
            label = "Select AETERM variable:",
            data_extract_spec = ui_args$ae_term,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_time_start"),
            label = "Select ASTDTM variable:",
            data_extract_spec = ui_args$ae_time_start,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ae_time_end"),
            label = "Select AENDTM variable:",
            data_extract_spec = ui_args$ae_time_end,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("cmtrt"),
            label = "Select CMTRT variable:",
            data_extract_spec = ui_args$cmtrt,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ds_time_start"),
            label = "Select TRTSDTM variable:",
            data_extract_spec = ui_args$ds_time_start,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("ds_time_end"),
            label = "Select TRTEDTM variable:",
            data_extract_spec = ui_args$ds_time_end,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("tabs"), "'] == 'Laboratory values'"),
        list(
          data_extract_input(
            id = ns("labor_paramcd"),
            label = "Select PARAMCD variable:",
            data_extract_spec = ui_args$labor_paramcd,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("labor_param"),
            label = "Select PARAM variable:",
            data_extract_spec = ui_args$labor_param,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("labor_timepoints"),
            label = "Select timepoints variable:",
            data_extract_spec = ui_args$labor_timepoints,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("labor_aval"),
            label = "Select AVAL variable:",
            data_extract_spec = ui_args$labor_aval,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("labor_avalu"),
            label = "Select AVALU variable:",
            data_extract_spec = ui_args$labor_avalu,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("anrind"),
            label = "Select ANRIND variable:",
            data_extract_spec = ui_args$anrind,
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

#' @importFrom timevis renderTimevis
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
                                  vitals_paramcd,
                                  vitals_param,
                                  labor_paramcd,
                                  labor_param,
                                  labor_timepoints,
                                  anrind,
                                  vitals_xaxis,
                                  vitals_aval,
                                  labor_aval,
                                  labor_avalu,
                                  atirel,
                                  medname_decoding,
                                  cmindc,
                                  cmdose,
                                  cmtrt,
                                  cmdosu,
                                  cmroute,
                                  cmdosfrq,
                                  cmstdy,
                                  cmendy,
                                  ae_term,
                                  ae_tox_grade,
                                  ae_causality,
                                  ae_outcome,
                                  ae_action,
                                  ae_time,
                                  ae_time_start,
                                  ae_time_end,
                                  ae_decod,
                                  ds_time_start,
                                  ds_time_end,
                                  plot_height,
                                  plot_width,
                                  label) {
  stopifnot(is_cdisc_data(datasets))

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
    chunks_push_data_merge(binf_merged_data(), chunks = call_stack)

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`

    call_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))


    my_calls <- template_basic_info(
      dataname = "ANL_FILTERED",
      binf_vars = input$`binf_vars-dataset_ADSL_singleextract-select`
    )

    mapply(expression = my_calls, call_stack_push)
    chunks_safe_eval(chunks = call_stack)
    call_stack
  })

  output$basic_info_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(basic_info_call())
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
    chunks_push_data_merge(mhist_merged_data(), chunks = mhist_stack)

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
    chunks_safe_eval(chunks = mhist_stack)
    mhist_stack
  })

  output$medical_history_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(mhist_call())
    chunks_get_var("result")
  })

  # Prior medication tab ----
  pmed_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(vitals_paramcd, vitals_xaxis, vitals_aval, atirel, medname_decoding),
    input_id = c("vitals_paramcd", "vitals_xaxis", "vitals_aval", "atirel", "medname_decoding"),
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
    chunks_push_data_merge(pmed_merged_data(), chunks = pmed_stack)

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
    chunks_safe_eval(pmed_stack)
    pmed_stack
  })

  output$prior_medication_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(pmed_call())
    chunks_get_var("result")
  })

  # Vitals tab ----
  vitals_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(vitals_paramcd, vitals_xaxis, vitals_aval),
    input_id = c("vitals_paramcd", "vitals_xaxis", "vitals_aval"),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  vitals_dat <- reactive({
    vitals_merged_data()$data()
  })

  output$vitals_paramcd_levels <- renderUI({
    paramcd_var <- input$`vitals_paramcd-dataset_ADVS_singleextract-select`
    paramcd_col <- vitals_dat()[[paramcd_var]]
    paramcd_col_levels <- if (is.factor(paramcd_col)) {
      levels(paramcd_col)
    }
    else {
      unique(paramcd_col)
    }

    tagList(selectInput(session$ns("vitals_paramcd_levels_vals"),
      "Select PARAMCD variable levels:",
      selected = paramcd_col_levels,
      choices = paramcd_col_levels, multiple = TRUE
    ))
  })

  vitals_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`vitals_paramcd-dataset_ADVS_singleextract-select`,
        "Please select PARAMCD variable."
      ),
      need(
        input$`vitals_paramcd_levels_vals`,
        "Please select PARAMCD variable levels."
      ),
      need(
        input$`vitals_xaxis-dataset_ADVS_singleextract-select`,
        "Please select Vitals x-axis variable."
      ),
      need(
        input$`vitals_aval-dataset_ADVS_singleextract-select`,
        "Please select AVAL variable."
      )
    )

    vitals_stack <- chunks$new()
    vitals_stack_push <- function(...) {
      chunks_push(..., chunks = vitals_stack)
    }
    chunks_push_data_merge(vitals_merged_data(), chunks = vitals_stack)

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`
    vitals_xaxis <- input$`vitals_xaxis-dataset_ADVS_singleextract-select`

    vitals_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    my_calls <- template_vitals(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      paramcd = input$`vitals_paramcd-dataset_ADVS_singleextract-select`,
      vitals_paramcd_levels = input$`vitals_paramcd_levels_vals`,
      vitals_xaxis = input$`vitals_xaxis-dataset_ADVS_singleextract-select`,
      aval = input$`vitals_aval-dataset_ADVS_singleextract-select`
    )

    mapply(expression = my_calls, vitals_stack_push)
    chunks_safe_eval(chunks = vitals_stack)
    vitals_stack
  })

  vitals_plot <- reactive({
    chunks_reset()
    chunks_push_chunks(vitals_call())
    chunks_get_var("result_plot")
  })

  # Therapy tab ----
  therapy_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(atirel, medname_decoding, cmindc, cmdose, cmtrt, cmdosu, cmroute, cmdosfrq, cmstdy, cmendy),
    input_id = c(
      "atirel", "medname_decoding", "cmindc", "cmdose", "cmtrt", "cmdosu", "cmroute", "cmdosfrq", "cmstdy", "cmendy"
    ),
    merge_function = "dplyr::left_join",
    anl_name = "ANL"
  )

  therapy_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`atirel-dataset_ADCM_singleextract-select`,
        "Please select ATIREL variable."
      ),
      need(
        input$`medname_decoding-dataset_ADCM_singleextract-select`,
        "Please select Medication decoding variable."
      ),
      need(
        input$`cmindc-dataset_ADCM_singleextract-select`,
        "Please select CMINDC variable."
      ),
      need(
        input$`cmdose-dataset_ADCM_singleextract-select`,
        "Please select CMDOSE variable."
      ),
      need(
        input$`cmtrt-dataset_ADCM_singleextract-select`,
        "Please select CMTRT variable."
      ),
      need(
        input$`cmdosu-dataset_ADCM_singleextract-select`,
        "Please select CMDOSU variable."
      ),
      need(
        input$`cmroute-dataset_ADCM_singleextract-select`,
        "Please select CMROUTE variable."
      ),
      need(
        input$`cmdosfrq-dataset_ADCM_singleextract-select`,
        "Please select CMDOSFRQ variable."
      ),
      need(
        input$`cmstdy-dataset_ADCM_singleextract-select`,
        "Please select CMSTDY variable."
      ),
      need(
        input$`cmendy-dataset_ADCM_singleextract-select`,
        "Please select CMENDY variable."
      )
    )

    therapy_stack <- chunks$new()
    therapy_stack_push <- function(...) {
      chunks_push(..., chunks = therapy_stack)
    }
    chunks_push_data_merge(therapy_merged_data(), chunks = therapy_stack)

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`

    therapy_stack_push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    my_calls <- template_therapy(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      atirel = input$`atirel-dataset_ADCM_singleextract-select`,
      medname_decoding = input$`medname_decoding-dataset_ADCM_singleextract-select`,
      cmtrt = input$`cmtrt-dataset_ADCM_singleextract-select`,
      cmdosu = input$`cmdosu-dataset_ADCM_singleextract-select`,
      cmroute = input$`cmroute-dataset_ADCM_singleextract-select`,
      cmdosfrq = input$`cmdosfrq-dataset_ADCM_singleextract-select`,
      cmstdy = input$`cmstdy-dataset_ADCM_singleextract-select`,
      cmendy = input$`cmendy-dataset_ADCM_singleextract-select`,
      cmindc = input$`cmindc-dataset_ADCM_singleextract-select`,
      cmdose = input$`cmdose-dataset_ADCM_singleextract-select`
    )

    mapply(expression = my_calls, therapy_stack_push)
    chunks_safe_eval(chunks = therapy_stack)
    therapy_stack
  })

  output$therapy_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(therapy_call())
    chunks_get_var("therapy_table")
  })

  therapy_plot <- reactive({
    chunks_reset()
    chunks_push_chunks(therapy_call())
    chunks_get_var("therapy_plot")
  })


  # Adverse events tab ----
  ae_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(ae_term, ae_tox_grade, ae_causality, ae_outcome, ae_action, ae_time, ae_decod),
    input_id = c("ae_term", "ae_tox_grade", "ae_causality", "ae_outcome", "ae_action", "ae_time", "ae_decod"),
    anl_name = "ANL"
  )

  ae_calls <- reactive({
    validate_checks()

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

    chunks_push_data_merge(ae_merged_data(), chunks = ae_stack)

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
      ae_time = input$`ae_time-dataset_ADAE_singleextract-select`,
      ae_decod = input$`ae_decod-dataset_ADAE_singleextract-select`
    )
    mapply(ae_calls, FUN = function(x) chunks_push(x, chunks = ae_stack))
    chunks_safe_eval(chunks = ae_stack)
    ae_stack
  })
  output$ae_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(ae_calls())
    chunks_get_var("ae_table")
  })

  ae_chart <- reactive({
    chunks_reset()
    chunks_push_chunks(ae_calls())
    chunks_get_var("ae_chart")
  })

  # Patient timeline tab ----
  patient_timeline_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(ae_term, ae_time_start, ae_time_end, ds_time_start, ds_time_end, cmtrt),
    input_id = c("ae_term", "ae_time_start", "ae_time_end", "ds_time_start", "ds_time_end", "cmtrt"),
    anl_name = "ANL"
  )

  patient_timeline_calls <- reactive({
    validate_checks()

    validate(
      need(
        input[[extract_input("ae_term", "ADAE")]],
        "Please select AETERM variable."
      ),
      need(
        input[[extract_input("ae_time_start", "ADAE")]],
        "Please select ASTDTM variable."
      ),
      need(
        input[[extract_input("ae_time_end", "ADAE")]],
        "Please select AENDTM variable."
      ),
      need(
        input[[extract_input("ds_time_start", "ADCM")]],
        "Please select CMASTDTM variable."
      ),
      need(
        input[[extract_input("ds_time_end", "ADCM")]],
        "Please select CMAENDTM variable."
      )
    )

    patient_timeline_stack <- chunks$new()
    chunks_push_data_merge(patient_timeline_merged_data(), chunks = patient_timeline_stack)

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`
    patient_timeline_stack$push(bquote({
      ANL_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    patient_timeline_calls <- template_patient_timeline(
      dataname = "ANL_FILTERED",
      patient_id = patient_id,
      ae_term = input$`ae_term-dataset_ADAE_singleextract-select`,
      ae_time_start = input$`ae_time_start-dataset_ADAE_singleextract-select`,
      ae_time_end = input$`ae_time_end-dataset_ADAE_singleextract-select`,
      ds_time_start = input$`ds_time_start-dataset_ADCM_singleextract-select`,
      ds_time_end = input$`ds_time_end-dataset_ADCM_singleextract-select`,
      cmtrt = input$`cmtrt-dataset_ADCM_singleextract-select`
    )

    mapply(patient_timeline_calls, FUN = function(x) chunks_push(x, chunks = patient_timeline_stack))
    chunks_safe_eval(chunks = patient_timeline_stack)
    patient_timeline_stack
  })

  output$patient_timeline_plot <- timevis::renderTimevis({
    chunks_reset()
    chunks_push_chunks(patient_timeline_calls())
    chunks_get_var("patient_timeline_plot")
  })


  # Laboratory values tab ----
  labor_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(labor_timepoints, labor_aval, labor_avalu, labor_param, labor_paramcd, anrind),
    input_id = c("labor_timepoints", "labor_aval", "labor_avalu", "labor_param", "labor_paramcd", "anrind"),
    anl_name = "ANL"
  )

  labor_calls <- reactive({
    validate_checks()

    validate(
      need(
        input[[extract_input("labor_timepoints", "ADLB")]],
        "Please select timepoints variable."
      ),
      need(
        input[[extract_input("labor_aval", "ADLB")]],
        "Please select AVAL variable."
      ),
      need(
        input[[extract_input("labor_avalu", "ADLB")]],
        "Please select AVALU variable."
      ),
      need(
        input[[extract_input("labor_param", "ADLB")]],
        "Please select PARAM variable."
      ),
      need(
        input[[extract_input("labor_paramcd", "ADLB")]],
        "Please select PARAMCD variable."
      ),
      need(
        input[[extract_input("anrind", "ADLB")]],
        "Please select ANRIND variable."
      )
    )

    labor_stack <- chunks$new()
    labor_stack$reset()

    chunks_push_data_merge(labor_merged_data(), chunks = labor_stack)

    patient_id <- input$`patient_id-dataset_ADSL_singleextract-select`
    labor_stack$push(bquote({
      ADLB_FILTERED <- ANL[ANL$USUBJID == .(patient_id), ] # nolint
    }))

    labor_calls <- template_laboratory(
      dataname = "ADLB_FILTERED",
      labor_timepoints = input[[extract_input("labor_timepoints", "ADLB")]],
      aval = input[[extract_input("labor_aval", "ADLB")]],
      avalu = input[[extract_input("labor_avalu", "ADLB")]],
      param = input[[extract_input("labor_param", "ADLB")]],
      paramcd = input[[extract_input("labor_paramcd", "ADLB")]],
      anrind = input[[extract_input("anrind", "ADLB")]],
    )

    mapply(labor_calls, FUN = function(x) chunks_push(x, chunks = labor_stack))
    chunks_safe_eval(chunks = labor_stack)
    labor_stack
  })

  output$lab_values <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(labor_calls())
    chunks_get_var("labor_table")
    },
    escape = FALSE
  )

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
    plot_with_settings_srv,
    id = "therapy_plot",
    plot_r = therapy_plot,
    height = plot_height,
    width = plot_width
  )

  # Swapping out global chunks ----
  # Make sure that get_chunks_object() has the code for the currently viewed tab
  observeEvent(input$tabs, handlerExpr = {
    chunks_reset()
    new_chunks <- switch(input$tabs,
      "Basic info" = basic_info_call(),
      "Medical history" = mhist_call(),
      "Prior medication" = pmed_call(),
      "Vitals" = vitals_call(),
      "Adverse events" = ae_calls(),
      "Laboratory values" = labor_calls(),
      "Patient timeline" = patient_timeline_calls()
    )
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
      vitals_aval,
      labor_aval,
      labor_aval,
      vitals_paramcd,
      vitals_param,
      labor_paramcd,
      labor_param,
      anrind,
      vitals_xaxis,
      labor_timepoints,
      atirel,
      medname_decoding,
      cmindc,
      cmdose,
      cmtrt,
      cmdosu,
      cmroute,
      cmdosfrq,
      cmstdy,
      cmendy,
      ae_term,
      ae_tox_grade,
      ae_causality,
      ae_outcome,
      ae_action,
      ae_time,
      ae_time_start,
      ae_time_end,
      ds_time_start,
      ds_time_end
    )),
    modal_title = label
  )
}
