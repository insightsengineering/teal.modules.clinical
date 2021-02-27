#' Template: Basic Info
#'
#' Creates a basic info template.
#'
#' @inheritParams template_arguments
#' @param bi_vars (`character`)\cr variable names to be shown in Basic Info tab.
#' @inheritParams tm_g_patient_profile
#'
template_basic_info <- function(dataname = "bi_merge",
                                bi_vars) {
  assert_that(
    is.string(dataname),
    is_character_vector(bi_vars)
  )
  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(
      expr = {
        values <- dataname %>%
          select(bi_vars) %>%
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
        bi_vars = bi_vars
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
#' @param mh_term (`character`)\cr name of reported name for medical history variable.
#' @param mh_bodsys (`character`)\cr name of body system or organ class variable.
#' @inheritParams tm_g_patient_profile
#'
template_medical_history <- function(dataname = "mh_merge",
                                     mh_term = "MHTERM",
                                     mh_bodsys = "MHBODSYS") {
  assert_that(
    is.string(dataname),
    is.string(mh_term),
    is.string(mh_bodsys)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      result <- # compared to the original app, MHDISTAT is not available in ADHM
        dataname %>%
        select(mh_bodsys, mh_term) %>%
        arrange(mh_bodsys) %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        distinct() %>%
        `colnames<-`(get_labels(dataname)$column_labels[c(mh_bodsys_char, mh_term_char)])
    }, env = list(
      dataname = as.name(dataname),
      mh_bodsys = as.name(mh_bodsys),
      mh_term = as.name(mh_term),
      mh_bodsys_char = mh_bodsys,
      mh_term_char = mh_term
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
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param medname_decoding (`character`)\cr name of standardized medication name variable.
#' @inheritParams tm_g_patient_profile

template_prior_medication <- function(dataname = "pm_merge",
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
#' @param v_paramcd (`character`)\cr name of the parameter code variable.
#' @param v_paramcd_levels (`character`)\cr (`paramcd`)\cr vector with (`v_paramcd`)\cr levels.
#' @param v_xaxis (`character`)\cr name of time variable used for the x-axis.
#' @param v_aval (`character`)\cr name of the analysis value variable.
#' @inheritParams tm_g_patient_profile
#'
template_vitals <- function(dataname = "v_merge",
                            v_paramcd = "PARAMCD",
                            v_paramcd_levels = c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP"),
                            v_xaxis = "ADY",
                            v_aval = "AVAL") {
  assert_that(
    is.string(dataname),
    is.string(v_paramcd),
    is.string(v_xaxis),
    is.string(v_aval)
  )
  # Note: VSDY (study day of vital signs) was replaced with ADY (analysis day)
  y <- list()
  y$plot <- list()

  vital_plot <- add_expr(
    list(),
    substitute(expr = {
      vitals <-
        dataname %>%
        group_by(paramcd, v_xaxis) %>%
        filter(paramcd %in% v_paramcd_levels_chars) %>%
        summarise(
          AVAL = max(aval, na.rm = T)
        )

      max_day <- max(vitals[[v_xaxis_char]], na.rm = T)
      max_aval <- max(vitals[[aval_char]], na.rm = T)
      max_aval_seq <- seq(0, max_aval, 10)

      provided_vita <- v_paramcd_levels_chars
      len_paramcd_levels_chars <- length(provided_vita)
      known_vita <- c("SYSBP", "DIABP", "TEMP", "RESP", "OXYSAT", "PULSE")

      v_paramcd_levels_e <- known_vita[na.omit(pmatch(provided_vita, known_vita))]
      len_v_paramcd_levels_e <- length(v_paramcd_levels_e)

      vars_colors <- color_palette(len_paramcd_levels_chars)
      names(vars_colors) <- provided_vita

      base_stats <- setNames(c(140, 90, 38, 20, 94, 100), known_vita)
      paramcd_stats_e <- base_stats[v_paramcd_levels_e]

      base_labels <- setNames(c("140mmHg", "90mmHg", "38\u00B0 C", "20/min", "94%", "100bpm"), known_vita)
      paramcd_labels_e <- base_labels[v_paramcd_levels_e]

      base_stats_df <- data.frame(
        x = rep(1, len_v_paramcd_levels_e),
        y = paramcd_stats_e,
        label = paramcd_labels_e,
        color = v_paramcd_levels_e
      )

      result_plot <- ggplot(data = vitals, mapping = aes(x = v_xaxis)) + # replaced VSDY
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
          breaks = seq(0, max(vitals[[v_xaxis_char]], na.rm = T), 50),
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
      paramcd = as.name(v_paramcd),
      paramcd_char = v_paramcd,
      v_paramcd_levels_chars = v_paramcd_levels,
      v_xaxis = as.name(v_xaxis),
      v_xaxis_char = v_xaxis,
      aval = as.name(v_aval),
      aval_char = v_aval
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
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param medname_decoding (`character`)\cr name of standardized medication name variable.
#' @param t_cmindc (`character`)\cr name of indication variable.
#' @param t_cmdose (`character`)\cr name of dose per administration variable.
#' @param t_cmtrt (`character`)\cr name of reported name of drug, med, or therapy variable.
#' @param t_cmdosu (`character`)\cr name of dose units variable.
#' @param t_cmroute (`character`)\cr name of route of administration variable.
#' @param t_cmdosfrq (`character`)\cr name of dosing frequency per interval variable.
#' @param t_cmstdy (`character`)\cr name of study day of start of medication variable.
#' @param t_cmendy (`character`)\cr name of study day of end of medication variable.
#' @inheritParams tm_g_patient_profile

template_therapy <- function(dataname = "t_merge",
                             atirel = "ATIREL",
                             medname_decoding = "CMDECOD",
                             t_cmindc = "CMINDC",
                             t_cmdose = "CMDOSE",
                             t_cmtrt = "CMTRT",
                             t_cmdosu = "CMDOSU",
                             t_cmroute = "CMROUTE",
                             t_cmdosfrq = "CMDOSFRQ",
                             t_cmstdy = "CMSTDY", # replaces t_cmstdy
                             t_cmendy = "CMENDY" # replaces t_cmendy
) {
  assert_that(
    is.string(dataname),
    is.string(atirel),
    is.string(medname_decoding),
    is.string(t_cmindc),
    is.string(t_cmdose),
    is.string(t_cmtrt),
    is.string(t_cmdosu),
    is.string(t_cmroute),
    is.string(t_cmdosfrq),
    is.string(t_cmstdy),
    is.string(t_cmendy)
  )

  y <- list()
  y$table_list <- list()
  y$plot_list <- list()

  #
  table_list <- add_expr(
    list(),
    substitute(expr = {
      cols_to_inlude <- c(
        t_cmindc_char,
        cmdecod_char,
        t_cmdose_char,
        t_cmtrt_char,
        t_cmdosu_char,
        t_cmroute_char,
        t_cmdosfrq_char,
        t_cmstdy_char,
        t_cmendy_char
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
        mutate(Dosage = paste(t_cmdose, t_cmdosu, t_cmdosfrq, t_cmroute)) %>%
        select(-t_cmdose, -t_cmdosu, -t_cmdosfrq, -t_cmroute) %>%
        select(t_cmindc, cmdecod, Dosage, everything()) %>%
        mutate(CMDECOD = case_when(
          nchar(as.character(cmdecod)) > 20 ~ as.character(t_cmtrt),
          TRUE ~ as.character(cmdecod)
        )) %>%
        select(-t_cmtrt) %>%
        arrange(t_cmindc, cmdecod, t_cmstdy) %>%
        distinct()
      # `colnames<-`(get_labels(dataname)$column_labels[t_cmindc, cmdecod, t_cmstdy]) # nolintr
    }, env = list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(medname_decoding),
      t_cmindc = as.name(t_cmindc),
      t_cmdose = as.name(t_cmdose),
      t_cmtrt = as.name(t_cmtrt),
      t_cmdosu = as.name(t_cmdosu),
      t_cmroute = as.name(t_cmroute),
      t_cmdosfrq = as.name(t_cmdosfrq),
      t_cmstdy = as.name(t_cmstdy),
      t_cmendy = as.name(t_cmendy),
      cmdecod_char = medname_decoding,
      t_cmindc_char = t_cmindc,
      t_cmdose_char = t_cmdose,
      t_cmtrt_char = t_cmtrt,
      t_cmdosu_char = t_cmdosu,
      t_cmroute_char = t_cmroute,
      t_cmdosfrq_char = t_cmdosfrq,
      t_cmendy_char = t_cmendy,
      t_cmstdy_char = t_cmstdy
    ))
  )

  plot_list <- add_expr(
    list(),
    substitute(expr = {
      max_day <- max(dataname[[t_cmendy_char]], na.rm = T)
      data <- dataname %>%
        filter(atirel %in% c("CONCOMITANT", "PRIOR")) %>% # remove PRIOR_CONCOMITANT
        select_at(cols_to_inlude) %>%
        filter(!is.na(cmdecod)) %>%
        mutate(DOSE = paste(t_cmdose, t_cmdosu, t_cmdosfrq)) %>%
        select(-t_cmdose, -t_cmdosu, -t_cmdosfrq) %>%
        select(t_cmindc, cmdecod, DOSE, everything()) %>%
        arrange(t_cmindc, cmdecod, t_cmstdy) %>%
        distinct() %>%
        mutate(CMSTDY = case_when(
          is.na(t_cmstdy) ~ 1,
          t_cmstdy < 1 ~ 1,
          TRUE ~ t_cmstdy
        )) %>%
        mutate(CMENDY = case_when(
          is.na(t_cmendy) ~ max_day,
          TRUE ~ t_cmendy
        )) %>%
        arrange(CMSTDY, desc(CMSTDY)) %>%
        mutate(CMDECOD = case_when(
          nchar(as.character(cmdecod)) > 20 ~ as.character(t_cmtrt),
          TRUE ~ as.character(cmdecod)
        ))

      therapy_plot <-
        ggplot(data = data, aes(fill = t_cmindc, color = t_cmindc, y = CMDECOD, x = CMSTDY)) +
        geom_segment(aes(xend = CMENDY, yend = CMDECOD), size = 2) +
        geom_text(
          data =
            data %>%
            select(CMDECOD, t_cmindc) %>%
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
      t_cmindc = as.name(t_cmindc),
      t_cmdose = as.name(t_cmdose),
      t_cmtrt = as.name(t_cmtrt),
      t_cmdosu = as.name(t_cmdosu),
      t_cmroute = as.name(t_cmroute),
      t_cmdosfrq = as.name(t_cmdosfrq),
      t_cmstdy = as.name(t_cmstdy),
      t_cmendy = as.name(t_cmendy),
      cmdecod_char = medname_decoding,
      t_cmindc_char = t_cmindc,
      t_cmdose_char = t_cmdose,
      t_cmtrt_char = t_cmtrt,
      t_cmdosu_char = t_cmdosu,
      t_cmroute_char = t_cmroute,
      t_cmdosfrq_char = t_cmdosfrq,
      t_cmstdy_char = t_cmstdy,
      t_cmendy_char = t_cmendy
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
#' @param ae_term (`character`)\cr name of the reported term for the adverse event variable.
#' @param ae_tox_grade (`character`)\cr name of the standard toxicity grade variable.
#' @param ae_causality (`character`)\cr name of the causality variable.
#' @param ae_outcome (`character`)\cr name of outcome of adverse event variable.
#' @param ae_action (`character`)\cr name of action taken with study treatment variable.
#' @param ae_time (`character`)\cr name of study day of start of adverse event variable.
#' @inheritParams tm_g_patient_profile
#'
template_adverse_events <- function(dataname = "ae_merge",
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
#' @param ae_term (`character`)\cr name of the reported term for the adverse event variable.
#' @param ae_time_start (`character`)\cr name of datetime start of adverse event variable.
#' @param ae_time_end (`character`)\cr name of datetime end of adverse event variable.
#' @param ds_time_start (`character`)\cr name of datetime first exposure to treatment variable.
#' @param ds_time_end (`character`)\cr name of datetime last exposure to treatment variable.
#' @param t_cmtrt (`character`)\cr name of reported name of drug, med, or therapy variable.
#'
template_patient_timeline <- function(dataname = "pt_merge",
                                      ae_term = "AETERM",
                                      ae_time_start = "ASTDTM", # to be updated
                                      ae_time_end = "AENDTM", # to be updated
                                      ds_time_start = "CMASTDTM", # to be updated
                                      ds_time_end = "CMAENDTM", # to be updated
                                      t_cmtrt = "CMTRT") {
  # Note: The variables used for ae_time_start, ae_time_end, ds_time_start and ds_time_end are to be updated after
  # random.cdisc.data updates.
  assert_that(
    is.string(dataname),
    is.string(ae_term) || is.null(ae_term),
    is.string(ae_time_start) || is.null(ae_time_start),
    is.string(ae_time_end) || is.null(ae_time_end),
    is.string(ds_time_start) || is.null(ds_time_start),
    is.string(ds_time_end) || is.null(ds_time_end),
    is.string(t_cmtrt) || is.null(t_cmtrt)
  )

  y <- list()
  y$chart <- list()

  chart_list <- add_expr(
    list(),
    substitute(
      expr = {
        # three sections are represented here: advers events, dosing and medication
        # adverse events
        ae_chart_vars_na <- any(
          vapply(list(ae_term_var, ae_time_start_var, ae_time_end_var), is.null, FUN.VALUE = logical(1))
          )
        if (ae_chart_vars_na) {
          ae_chart <- dataname[FALSE, ]
        } else {
          ae_chart <- dataname %>%
            select(ae_term, ae_time_start, ae_time_end) %>%
            distinct()
        }

        # dosing
        ds_chart_vars_na <- any(vapply(list(ds_time_start_var, ds_time_end_var), is.null, FUN.VALUE = logical(1)))
        if (ds_chart_vars_na) {
          ds_chart <- dataname[FALSE, ]
        } else {
          ds_chart <- dataname %>%
            select(ds_time_start, ds_time_end) %>%
            distinct() %>%
            mutate(
              label_start = "First Exposure to Treatment",
              label_end = "Last Exposure to Treatment"
            )
        }

        # medication
        med_chart_vars_na <- any(
          vapply(list(t_cmtrt_var, ds_time_start_var, ds_time_end_var), is.null, FUN.VALUE = logical(1))
          )
        if (med_chart_vars_na) {
          med_chart <- dataname[FALSE, ]
        } else {
          med_chart <- dataname %>%
            select(t_cmtrt, ds_time_start, ds_time_end) %>%
            distinct()
        }

        min_ds_chart_time_start <- if (ds_chart_vars_na) c() else min(ds_chart[[ds_time_start_var]])
        min_ds_chart_time_end <- if (ds_chart_vars_na) c() else min(ds_chart[[ds_time_end_var]])
        max_ds_chart_time_end <- if (ds_chart_vars_na) c() else  max(ds_chart[[ds_time_end_var]])

        timevis_data <- data.frame(
          id = seq_len(nrow(ae_chart) +
            length(unique(ds_chart[["label_start"]])) +
            length(unique(ds_chart[["label_end"]])) +
            nrow(med_chart)),
          content = c(
            if (ae_chart_vars_na) c() else as.character(ae_chart[[ae_term_var]]),
            as.character(unique(ds_chart[["label_start"]])),
            as.character(unique(ds_chart[["label_end"]])),
            if (med_chart_vars_na) c() else as.character(med_chart[[t_cmtrt_var]])
          ),
          start = c(
            if (ae_chart_vars_na) c() else ae_chart[[ae_time_start_var]],
            min_ds_chart_time_start,
            max_ds_chart_time_end,
            if (med_chart_vars_na) c() else med_chart[[ds_time_start_var]]
          ),
          end = c(
            if (ae_chart_vars_na) c() else ae_chart[[ae_time_end_var]],
            min_ds_chart_time_start,
            max_ds_chart_time_end,
            if (med_chart_vars_na) c() else med_chart[[ds_time_end_var]]
          ),
          group = c(
            rep("AE", if (ae_chart_vars_na) 0 else length(ae_chart[[ae_term_var]])),
            rep("DOS", length(min_ds_chart_time_start)),
            rep("DOS", length(min_ds_chart_time_end)),
            rep("MED", if (med_chart_vars_na) 0 else length(med_chart[[t_cmtrt_var]]))
          ),
          type = c(
            rep("range", if (ae_chart_vars_na) 0 else length(ae_chart[[ae_term_var]])),
            rep("point", length(min_ds_chart_time_start)),
            rep("point", length(min_ds_chart_time_end)),
            rep("range", if (med_chart_vars_na) 0 else length(med_chart[[t_cmtrt_var]]))
          )
        )

        # in some cases, dates are converted to numeric so this is a step to convert them back

        posixct_origin <- "1970-01-01 00:00.00 UTC"
        timevis_data_empty <- nrow(timevis_data) == 0
        timevis_data$start <- if (!timevis_data_empty) as.POSIXct(timevis_data$start, origin = posixct_origin)
        timevis_data$end <- if (!timevis_data_empty) as.POSIXct(timevis_data$end, origin = posixct_origin)

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
        ae_term = if (is.null(ae_term)) ae_term else as.name(ae_term),
        ae_time_start = if (is.null(ae_time_start)) ae_time_start else as.name(ae_time_start),
        ae_time_end = if (is.null(ae_time_end)) ae_time_end else as.name(ae_time_end),
        ds_time_start = if (is.null(ds_time_start)) ds_time_start else as.name(ds_time_start),
        ds_time_end = if (is.null(ds_time_end)) ds_time_end else as.name(ds_time_end),
        t_cmtrt = if (is.null(t_cmtrt)) t_cmtrt else as.name(t_cmtrt),
        ae_term_var = ae_term,
        ae_time_start_var = ae_time_start,
        ae_time_end_var = ae_time_end,
        ds_time_start_var = ds_time_start,
        ds_time_end_var = ds_time_end,
        t_cmtrt_var = t_cmtrt
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
#' Creates a laboratory template.
#' @inheritParams template_arguments
#' @param lb_paramcd (`character`)\cr name of the parameter code variable.
#' @param lb_param (`character`)\cr name of the parameter variable.
#' @param lb_timepoints  (`character`)\cr name of time variable used for
#' the laboratory table.
#' @param lb_anrind (`character`)\cr name of the analysis reference range indicator variable.
#' @param lb_aval (`character`)\cr name of the analysis value variable.
#' @param lb_avalu (`character`)\cr name of the analysis value unit variable.
#' @inheritParams tm_g_patient_profile
#'
template_laboratory <- function(dataname = "lb_merge",
                                lb_paramcd = "PARAMCD",
                                lb_param = "PARAM",
                                lb_anrind = "ANRIND",
                                lb_timepoints = "ADY",
                                lb_aval = "AVAL",
                                lb_avalu = "AVALU") {
  assert_that(
    is.string(dataname),
    is.string(lb_paramcd),
    is.string(lb_param),
    is.string(lb_anrind),
    is.string(lb_timepoints),
    is.string(lb_aval),
    is.string(lb_avalu)
  )

  y <- list()
  y$table <- list()

  table_lab_list <- add_expr(
    list(),
    substitute({
      labor_table <- dataname %>%
        select(lb_timepoints, paramcd, param, aval, avalu, lb_anrind) %>%
        arrange(lb_timepoints) %>%
        select(-lb_timepoints) %>%
        mutate(aval_lb_anrind = color_lab_values(paste(aval, lb_anrind))) %>%
        select(-c(aval, lb_anrind)) %>%
        group_by(paramcd, param) %>%
        mutate(INDEX = row_number()) %>%
        ungroup() %>%
        tidyr::pivot_wider(names_from = INDEX, values_from = aval_lb_anrind) %>%
        mutate(PARAM = stringr::str_replace_all(param, "\\(.*?\\)", "")) %>%
        mutate(PARAM = stringr::str_squish(param)) %>%
        mutate(PARAM = stringr::str_trunc(param, width = 20))
      },
      env = list(
        dataname = as.name(dataname),
        param = as.name(lb_param),
        paramcd = as.name(lb_paramcd),
        aval = as.name(lb_aval),
        avalu = as.name(lb_avalu),
        lb_timepoints = as.name(lb_timepoints),
        lb_anrind = as.name(lb_anrind)
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
#' @param patient_col (`character`) value patient ID column to be used.
#' @param bi_vars (`choices selected` or `data_extract_input`)\cr ADSL columns to be shown in Basic Info tab.
#' @param ae_term (`choices selected` or `data_extract_input`)\cr \code{AETERM} column of the ADAE dataset.
#' @param ae_tox_grade (`choices selected` or `data_extract_input`)\cr \code{AETOXGR} column of the ADAE dataset.
#' @param ae_causality (`choices selected` or `data_extract_input`)\cr \code{AEREL} column of the ADAE dataset.
#' @param ae_outcome (`choices selected` or `data_extract_input`)\cr \code{AEOUT} column of the ADAE dataset.
#' @param ae_action (`choices selected` or `data_extract_input`)\cr \code{AEACN} column of the ADAE dataset.
#' @param ae_time (`choices selected` or `data_extract_input`)\cr \code{ASTDY} column of the ADAE dataset.
#' @param ae_time_start (`choices selected` or `data_extract_input`)\cr \code{ASTDTM} column of the AE
#' start of the ADAE dataset.
#' @param ae_time_end (`choices selected` or `data_extract_input`)\cr \code{AENDTM} column of the AE
#' end of the ADAE dataset.
#' @param ae_decod (`choices selected` or `data_extract_input`)\cr \code{AEDECOD} column of the ADAE dataset.
#' @param mh_term (`choices selected` or `data_extract_input`)\cr \code{MHTERM} column of the ADMH dataset.
#' @param mh_bodsys (`choices selected` or `data_extract_input`)\cr \code{MHBODSYS} column of the ADMH dataset.
#' @param v_paramcd (`choices selected` or `data_extract_input`)\cr \code{PARAMCD} column of the ADVS dataset.
#' @param v_param (`choices selected` or `data_extract_input`)\cr \code{PARAM} column of the ADVS dataset.
#' @param v_aval (`choices selected` or `data_extract_input`)\cr \code{AVAL} column of the ADVS dataset.
#' @param v_xaxis (`choices selected` or `data_extract_input`)\cr Time variable to be represented in
#' the vitals plot x-axis.
#' @param lb_paramcd (`choices selected` or `data_extract_input`)\cr \code{PARAMCD} column of the ADLB dataset.
#' @param lb_param (`choices selected` or `data_extract_input`)\cr \code{PARAM} column of the ADLB dataset.
#' @param lb_timepoints (`choices selected` or `data_extract_input`)\cr Time variable to be represented in
#' the laboratory table.
#' @param lb_anrind (`choices selected` or `data_extract_input`)\cr \code{ANRIND} column of the ADLB dataset
#' with 3 possible levels "HIGH", "LOW" and "NORMAL".
#' @param lb_aval (`choices selected` or `data_extract_input`)\cr \code{AVAL} column of the ADLB dataset.
#' @param lb_avalu (`choices selected` or `data_extract_input`)\cr \code{AVALU} column of the ADLB dataset.
#' @param atirel (`choices selected` or `data_extract_input`)\cr \code{ATIREL} column of the ADCM dataset.
#' @param medname_decoding (`choices selected` or `data_extract_input`)\cr \code{CMDECOD} column of the ADCM dataset.
#' @param t_cmindc (`choices selected` or `data_extract_input`)\cr \code{CMINDC} column of the ADCM dataset.
#' @param t_cmdose (`choices selected` or `data_extract_input`)\cr \code{CMDOSE} column of the ADCM dataset.
#' @param t_cmtrt (`choices selected` or `data_extract_input`)\cr \code{CMTRT} column of the ADCM dataset.
#' @param t_cmdosu (`choices selected` or `data_extract_input`)\cr \code{CMDOSU} column of the ADCM dataset.
#' @param t_cmroute (`choices selected` or `data_extract_input`)\cr \code{CMROUTE} column of the ADCM dataset.
#' @param t_cmdosfrq (`choices selected` or `data_extract_input`)\cr \code{CMDOSFRQ} column of the ADCM dataset.
#' @param t_cmstdy (`choices selected` or `data_extract_input`)\cr \code{CMSTDY} column of the ADCM dataset.
#' @param t_cmendy (`choices selected` or `data_extract_input`)\cr \code{CMENDY} column of the ADCM dataset.
#' @param ds_time_start (`choices selected` or `data_extract_input`)\cr \code{CMASTDTM} column of treatment
#' start of the ADCM dataset.
#' @param ds_time_end (`choices selected` or `data_extract_input`)\cr \code{CMAENDTM} column of treatment
#' end of the ADCM dataset.
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
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- radae(cached = TRUE)"),
#'     cdisc_dataset("ADMH", ADMH, code = "ADMH <- radmh(cached = TRUE)"),
#'     cdisc_dataset("ADCM", ADCM, code = 'ADCM <- radcm(cached = TRUE)
#'       ADCM$CMINDC <- ADCM$CMCAT
#'       ADCM$CMDECOD <- ADCM$CMCAT
#'       ADCM$CMDOSE <- 1
#'       ADCM$CMTRT <- ADCM$CMCAT
#'       ADCM$CMDOSU <- "U"
#'       ADCM$CMROUTE <- "CMROUTE"
#'       ADCM$CMDOSFRQ <- "CMDOSFRQ"
#'       ADCM$CMSTDY <- 1
#'       ADCM[ADCM$CMCAT == "medcl B", ]$CMSTDY <- 20
#'       ADCM[ADCM$CMCAT == "medcl C", ]$CMSTDY <- 150
#'       ADCM$CMENDY <- 500
#'       ADCM[ADCM$CMCAT == "medcl B", ]$CMENDY <- 700
#'       ADCM[ADCM$CMCAT == "medcl C", ]$CMENDY <- 1000
#'       ADCM$CMASTDTM <- ADCM$ASTDTM
#'       ADCM$CMAENDTM <- ADCM$AENDTM',
#'       keys = adcm_keys),
#'     cdisc_dataset("ADVS", ADVS, code = "ADVS <- radvs(cached = TRUE)"),
#'     cdisc_dataset("ADLB", ADLB, code = "ADLB <- radlb(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_patient_profile(
#'       label = "Patient Profile",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       bi_vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL),
#'           selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       mh_term = data_extract_spec(
#'         dataname = "ADMH",
#'         select = select_spec(
#'           choices = variable_choices(ADMH, c("MHTERM")),
#'           selected = c("MHTERM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       mh_bodsys = data_extract_spec(
#'         dataname = "ADMH",
#'         select = select_spec(
#'           choices = variable_choices(ADMH, "MHBODSYS"),
#'           selected = c("MHBODSYS"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       v_paramcd = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "PARAMCD"),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       v_param = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "PARAM"),
#'           selected = c("PARAM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       lb_paramcd = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "PARAMCD"),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       lb_param = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "PARAM"),
#'           selected = c("PARAM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       v_xaxis = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "ADY"),
#'           selected = c("ADY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       lb_timepoints = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "ADY"),
#'           selected = c("ADY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       lb_anrind = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "ANRIND"),
#'           selected = c("ANRIND"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       v_aval = data_extract_spec(
#'         dataname = "ADVS",
#'         select = select_spec(
#'           choices = variable_choices(ADVS, "AVAL"),
#'           selected = c("AVAL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       lb_aval = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = variable_choices(ADLB, "AVAL"),
#'           selected = c("AVAL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       lb_avalu = data_extract_spec(
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
#'       t_cmindc = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMINDC"),
#'           selected = c("CMINDC"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmdose = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDOSE"),
#'           selected = c("CMDOSE"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmtrt = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMTRT"),
#'           selected = c("CMTRT"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmdosu = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDOSU"),
#'           selected = c("CMDOSU"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmroute = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMROUTE"),
#'           selected = c("CMROUTE"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmdosfrq = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMDOSFRQ"),
#'           selected = c("CMDOSFRQ"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmstdy = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMSTDY"),
#'           selected = c("CMSTDY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       t_cmendy = data_extract_spec(
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
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_tox_grade = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AETOXGR"),
#'           selected = c("AETOXGR"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_causality = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AEREL"),
#'           selected = c("AEREL"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_outcome = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AEOUT"),
#'           selected = c("AEOUT"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_action = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AEACN"),
#'           selected = c("AEACN"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_time = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "ASTDY"),
#'           selected = c("ASTDY"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_decod = NULL,
#'       ae_time_start = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "ASTDTM"),
#'           selected = c("ASTDTM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ae_time_end = data_extract_spec(
#'         dataname = "ADAE",
#'         select = select_spec(
#'           choices = variable_choices(ADAE, "AENDTM"),
#'           selected = c("AENDTM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ds_time_start = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMASTDTM"),
#'           selected = c("CMASTDTM"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       ds_time_end = data_extract_spec(
#'         dataname = "ADCM",
#'         select = select_spec(
#'           choices = variable_choices(ADCM, "CMAENDTM"),
#'           selected = c("CMAENDTM"),
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
                                 parentname = "ADSL",
                                 patient_col = "USUBJID",
                                 bi_vars = NULL,
                                 mh_term = NULL,
                                 mh_bodsys = NULL,
                                 atirel = NULL,
                                 medname_decoding = NULL,
                                 v_paramcd = NULL,
                                 v_param = NULL,
                                 v_xaxis = NULL,
                                 v_aval = NULL,
                                 lb_paramcd = NULL,
                                 lb_param = NULL,
                                 lb_timepoints = NULL,
                                 lb_anrind = NULL,
                                 lb_aval = NULL,
                                 lb_avalu = NULL,
                                 ae_term = NULL,
                                 ae_tox_grade = NULL,
                                 ae_causality = NULL,
                                 ae_outcome = NULL,
                                 ae_action = NULL,
                                 ae_time = NULL,
                                 ae_time_start = NULL,
                                 ae_time_end = NULL,
                                 ae_decod = NULL,
                                 t_cmindc = NULL,
                                 t_cmdose = NULL,
                                 t_cmtrt = NULL,
                                 t_cmdosu = NULL,
                                 t_cmroute = NULL,
                                 t_cmdosfrq = NULL,
                                 t_cmstdy = NULL,
                                 t_cmendy = NULL,
                                 ds_time_start = NULL,
                                 ds_time_end = NULL,
                                 plot_height = c(700L, 200L, 2000L),
                                 plot_width = c(900L, 200L, 2000L),
                                 pre_output = NULL,
                                 post_output = NULL) {
  assert_that(is_character_single(label))
  assert_that(is_character_single(parentname))
  assert_that(is_character_single(patient_col))
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
    bi_vars = if_not_null(bi_vars, cs_to_des_select(bi_vars, dataname = parentname)),
    mh_term = if_not_null(mh_term, cs_to_des_select(mh_term, dataname = parentname)),
    mh_bodsys = if_not_null(mh_bodsys, cs_to_des_select(mh_bodsys, dataname = parentname)),
    v_paramcd = if_not_null(v_paramcd, cs_to_des_select(v_paramcd, dataname = parentname)),
    v_param = if_not_null(v_param, cs_to_des_select(v_param, dataname = parentname)),
    lb_paramcd = if_not_null(lb_paramcd, cs_to_des_select(lb_paramcd, dataname = parentname)),
    lb_param = if_not_null(lb_param, cs_to_des_select(lb_param, dataname = parentname)),
    v_xaxis = if_not_null(v_xaxis, cs_to_des_select(v_xaxis, dataname = parentname)),
    lb_timepoints = if_not_null(lb_timepoints, cs_to_des_select(lb_timepoints, dataname = parentname)),
    lb_anrind = if_not_null(lb_anrind, cs_to_des_select(lb_anrind, dataname = parentname)),
    v_aval = if_not_null(v_aval, cs_to_des_select(v_aval, dataname = parentname)),
    lb_aval = if_not_null(lb_aval, cs_to_des_select(lb_aval, dataname = parentname)),
    lb_avalu = if_not_null(lb_avalu, cs_to_des_select(lb_avalu, dataname = parentname)),
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
    t_cmindc = if_not_null(t_cmindc, cs_to_des_select(t_cmindc, dataname = parentname)),
    t_cmdose = if_not_null(t_cmdose, cs_to_des_select(t_cmdose, dataname = parentname)),
    t_cmtrt = if_not_null(t_cmtrt, cs_to_des_select(t_cmtrt, dataname = parentname)),
    t_cmdosu = if_not_null(t_cmdosu, cs_to_des_select(t_cmdosu, dataname = parentname)),
    t_cmdosfrq = if_not_null(t_cmdosfrq, cs_to_des_select(t_cmdosfrq, dataname = parentname)),
    t_cmroute = if_not_null(t_cmroute, cs_to_des_select(t_cmroute, dataname = parentname)),
    t_cmstdy = if_not_null(t_cmstdy, cs_to_des_select(t_cmstdy, dataname = parentname)),
    t_cmendy = if_not_null(t_cmendy, cs_to_des_select(t_cmendy, dataname = parentname)),
    ds_time_start = if_not_null(ds_time_start, cs_to_des_select(ds_time_start, dataname = parentname)),
    ds_time_end = if_not_null(ds_time_end, cs_to_des_select(ds_time_end, dataname = parentname))
  )
  assert_that(is.null(bi_vars) || is.cs_or_des(bi_vars))
  assert_that(is.null(mh_term) || is.cs_or_des(mh_term))
  assert_that(is.null(mh_bodsys) || is.cs_or_des(mh_bodsys))
  assert_that(is.null(v_paramcd) || is.cs_or_des(v_paramcd))
  assert_that(is.null(lb_paramcd) || is.cs_or_des(lb_paramcd))
  assert_that(is.null(v_xaxis) || is.cs_or_des(v_xaxis))
  assert_that(is.null(v_aval) || is.cs_or_des(v_aval))
  assert_that(is.null(lb_aval) || is.cs_or_des(lb_aval))
  assert_that(is.null(atirel) || is.cs_or_des(atirel))
  assert_that(is.null(medname_decoding) || is.cs_or_des(medname_decoding))
  assert_that(is.null(ae_term) || is.cs_or_des(ae_term))
  assert_that(is.null(ae_tox_grade) || is.cs_or_des(ae_tox_grade))
  assert_that(is.null(ae_causality) || is.cs_or_des(ae_causality))
  assert_that(is.null(ae_outcome) || is.cs_or_des(ae_outcome))
  assert_that(is.null(ae_action) || is.cs_or_des(ae_action))
  assert_that(is.null(ae_time) || is.cs_or_des(ae_time))
  assert_that(is.null(ae_time_end) || is.cs_or_des(ae_time_end))
  assert_that(is.null(t_cmindc) || is.cs_or_des(t_cmindc))
  assert_that(is.null(t_cmdose) || is.cs_or_des(t_cmdose))
  assert_that(is.null(t_cmtrt) || is.cs_or_des(t_cmtrt))
  assert_that(is.null(t_cmdosu) || is.cs_or_des(t_cmdosu))
  assert_that(is.null(t_cmdosfrq) || is.cs_or_des(t_cmdosfrq))
  assert_that(is.null(t_cmroute) || is.cs_or_des(t_cmroute))
  assert_that(is.null(t_cmstdy) || is.cs_or_des(t_cmstdy))
  assert_that(is.null(t_cmendy) || is.cs_or_des(t_cmendy))
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
        parentname = parentname,
        label = label,
        patient_col = patient_col,
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
  is_single_dataset_value <- is_single_dataset(
    ui_args$bi_vars,
    ui_args$mh_term,
    ui_args$mh_bodsys,
    ui_args$v_paramcd,
    ui_args$v_param,
    ui_args$lb_paramcd,
    ui_args$lb_param,
    ui_args$v_xaxis,
    ui_args$lb_timepoints,
    ui_args$anrind,
    ui_args$v_aval,
    ui_args$lb_aval,
    ui_args$lb_avalu,
    ui_args$atirel,
    ui_args$medname_decoding,
    ui_args$ae_term,
    ui_args$ae_tox_grade,
    ui_args$ae_causality,
    ui_args$ae_outcome,
    ui_args$ae_action,
    ui_args$ae_time,
    ui_args$ae_time_start,
    ui_args$ae_time_end,
    ui_args$ae_decod,
    ui_args$t_cmindc,
    ui_args$t_cmdose,
    ui_args$t_cmtrt,
    ui_args$t_cmdosu,
    ui_args$t_cmdosfrq,
    ui_args$t_cmroute,
    ui_args$t_cmstdy,
    ui_args$t_cmendy,
    ui_args$ds_time_start,
    ui_args$ds_time_end)

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
      datanames_input(ui_args[
        c("bi_vars",
          "mh_term",
          "mh_bodsys",
          "v_paramcd",
          "v_param",
          "lb_paramcd",
          "lb_param",
          "v_xaxis",
          "lb_timepoints",
          "anrind",
          "v_aval",
          "lb_aval",
          "lb_avalu",
          "atirel",
          "medname_decoding",
          "ae_term",
          "ae_tox_grade",
          "ae_causality",
          "ae_outcome",
          "ae_action",
          "ae_time",
          "ae_time_start",
          "ae_time_end",
          "ae_decod",
          "t_cmindc",
          "t_cmdose",
          "t_cmtrt",
          "t_cmdosu",
          "t_cmdosfrq",
          "t_cmroute",
          "t_cmstdy",
          "t_cmendy",
          "ds_time_start",
          "ds_time_end"
        )]
      ),
      tags$span(class = "help-block", HTML(sprintf("Dataset: <code>%s</code>", ui_args$parentname))),
      optionalSelectInput(ns("patient_id"), "Select Patient:"),
      div(
        id = ns("bi_vars-div"),
        data_extract_input(
          id = ns("bi_vars"),
          label = "Select variable:",
          data_extract_spec = ui_args$bi_vars,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("mh_term-div"),
        data_extract_input(
          id = ns("mh_term"),
          label = "Select MHTERM variable:",
          data_extract_spec = ui_args$mh_term,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("mh_bodsys-div"),
        data_extract_input(
          id = ns("mh_bodsys"),
          label = "Select MHBODSYS variable:",
          data_extract_spec = ui_args$mh_bodsys,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("medname_decoding-div"),
        data_extract_input(
          id = ns("medname_decoding"),
          label = "Select the medication decoding column:",
          data_extract_spec = ui_args$medname_decoding,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("v_paramcd-div"),
        data_extract_input(
          id = ns("v_paramcd"),
          label = "Select PARAMCD variable:",
          data_extract_spec = ui_args$v_paramcd,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("v_paramcd_levels-div"),
        uiOutput(ns("v_paramcd_levels"))
      ),
      div(
        id = ns("v_xaxis-div"),
        data_extract_input(
          id = ns("v_xaxis"),
          label = "Select vital plot x-axis:",
          data_extract_spec = ui_args$v_xaxis,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("v_aval-div"),
        data_extract_input(
          id = ns("v_aval"),
          label = "Select AVAL variable:",
          data_extract_spec = ui_args$v_aval,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("atirel-div"),
        data_extract_input(
          id = ns("atirel"),
          label = "Select ATIREL variable:",
          data_extract_spec = ui_args$atirel,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmindc-div"),
        data_extract_input(
          id = ns("t_cmindc"),
          label = "Select CMINDC variable:",
          data_extract_spec = ui_args$t_cmindc,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmdose-div"),
        data_extract_input(
          id = ns("t_cmdose"),
          label = "Select CMDOSE variable:",
          data_extract_spec = ui_args$t_cmdose,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmtrt-div"),
        data_extract_input(
          id = ns("t_cmtrt"),
          label = "Select CMTRT variable:",
          data_extract_spec = ui_args$t_cmtrt,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmdosu-div"),
        data_extract_input(
          id = ns("t_cmdosu"),
          label = "Select CMDOSU variable:",
          data_extract_spec = ui_args$t_cmdosu,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmroute-div"),
        data_extract_input(
          id = ns("t_cmroute"),
          label = "Select CMROUTE variable:",
          data_extract_spec = ui_args$t_cmroute,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmdosfrq-div"),
        data_extract_input(
          id = ns("t_cmdosfrq"),
          label = "Select CMDOSFRQ variable:",
          data_extract_spec = ui_args$t_cmdosfrq,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmstdy-div"),
        data_extract_input(
          id = ns("t_cmstdy"),
          label = "Select CMSTDY variable:",
          data_extract_spec = ui_args$t_cmstdy,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("t_cmendy-div"),
        data_extract_input(
          id = ns("t_cmendy"),
          label = "Select CMENDY variable:",
          data_extract_spec = ui_args$t_cmendy,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_term-div"),
        data_extract_input(
          id = ns("ae_term"),
          label = "Select AETERM variable:",
          data_extract_spec = ui_args$ae_term,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_tox_grade-div"),
        data_extract_input(
          id = ns("ae_tox_grade"),
          label = "Select AETOXGR variable:",
          data_extract_spec = ui_args$ae_tox_grade,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_causality-div"),
        data_extract_input(
          id = ns("ae_causality"),
          label = "Select AEREL variable:",
          data_extract_spec = ui_args$ae_causality,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_outcome-div"),
        data_extract_input(
          id = ns("ae_outcome"),
          label = "Select AEOUT variable:",
          data_extract_spec = ui_args$ae_outcome,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_action-div"),
        data_extract_input(
          id = ns("ae_action"),
          label = "Select AEACN variable:",
          data_extract_spec = ui_args$ae_action,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_time-div"),
        data_extract_input(
          id = ns("ae_time"),
          label = "Select ASTDY variable:",
          data_extract_spec = ui_args$ae_time,
          is_single_dataset = is_single_dataset_value
        )
      ),
      if_not_null(
        ui_args$ae_decod,
        div(
          id = ns("ae_decod-div"),
          data_extract_input(
            id = ns("ae_decod"),
            label = "Select DECOD variable:",
            data_extract_spec = ui_args$ae_decod,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      div(
        id = ns("ae_time_start-div"),
        data_extract_input(
          id = ns("ae_time_start"),
          label = "Select ASTDTM variable:",
          data_extract_spec = ui_args$ae_time_start,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ae_time_end-div"),
        data_extract_input(
          id = ns("ae_time_end"),
          label = "Select AENDTM variable:",
          data_extract_spec = ui_args$ae_time_end,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ds_time_start-div"),
        data_extract_input(
          id = ns("ds_time_start"),
          label = "Select TRTSDTM variable:",
          data_extract_spec = ui_args$ds_time_start,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("ds_time_end-div"),
        data_extract_input(
          id = ns("ds_time_end"),
          label = "Select TRTEDTM variable:",
          data_extract_spec = ui_args$ds_time_end,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("lb_paramcd-div"),
        data_extract_input(
          id = ns("lb_paramcd"),
          label = "Select PARAMCD variable:",
          data_extract_spec = ui_args$lb_paramcd,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("lb_param-div"),
        data_extract_input(
          id = ns("lb_param"),
          label = "Select PARAM variable:",
          data_extract_spec = ui_args$lb_param,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("lb_timepoints-div"),
        data_extract_input(
          id = ns("lb_timepoints"),
          label = "Select timepoints variable:",
          data_extract_spec = ui_args$lb_timepoints,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("lb_aval-div"),
        data_extract_input(
          id = ns("lb_aval"),
          label = "Select AVAL variable:",
          data_extract_spec = ui_args$lb_aval,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("lb_avalu-div"),
        data_extract_input(
          id = ns("lb_avalu"),
          label = "Select AVALU variable:",
          data_extract_spec = ui_args$lb_avalu,
          is_single_dataset = is_single_dataset_value
        )
      ),
      div(
        id = ns("lb_anrind-div"),
        data_extract_input(
          id = ns("lb_anrind"),
          label = "Select ANRIND variable:",
          data_extract_spec = ui_args$lb_anrind,
          is_single_dataset = is_single_dataset_value
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
                                  parentname,
                                  patient_col,
                                  bi_vars,
                                  mh_term,
                                  mh_bodsys,
                                  v_paramcd,
                                  v_param,
                                  lb_paramcd,
                                  lb_param,
                                  lb_timepoints,
                                  lb_anrind,
                                  v_xaxis,
                                  v_aval,
                                  lb_aval,
                                  lb_avalu,
                                  atirel,
                                  medname_decoding,
                                  t_cmindc,
                                  t_cmdose,
                                  t_cmtrt,
                                  t_cmdosu,
                                  t_cmroute,
                                  t_cmdosfrq,
                                  t_cmstdy,
                                  t_cmendy,
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

  patient_id <- reactive(input$patient_id)

  # global checks
  validate_checks <- reactive({
    validate(
      need(
        patient_id(),
        "Please select Patient ID."
      )
    )
  })

  patient_data_base <- reactive(unique(datasets$get_data(parentname, filtered = TRUE)[[patient_col]]))
  # Init
  unique_patients <- patient_data_base()
  updateOptionalSelectInput(session, "patient_id", choices = unique_patients, selected = unique_patients[1])

  observeEvent(patient_data_base(), {
    patient_id_s <- patient_id()
    unique_patients <- patient_data_base()
    patient_selected <- if (is.null(patient_id_s) || !patient_id_s %in% unique_patients) {
      NULL
    } else {
      patient_id_s
    }
    updateOptionalSelectInput(session, "patient_id", choices = unique_patients, selected = patient_selected)
  }, ignoreInit = TRUE)

  # Basic Info tab ----
  binf_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(bi_vars),
    input_id = "bi_vars",
    merge_function = "dplyr::left_join",
    anl_name = "bi_merge"
  )

  basic_info_call <- reactive({
    validate_checks()
    validate(
      need(
        input$`bi_vars-dataset_ADSL_singleextract-select`,
        "Please select basic info variables."
      )
    )

    call_stack <- chunks$new()
    call_stack_push <- function(...) {
      chunks_push(..., chunks = call_stack)
    }
    chunks_push_data_merge(binf_merged_data(), chunks = call_stack)

    call_stack_push(bquote({
      bi_merge <- bi_merge[bi_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))


    my_calls <- template_basic_info(
      dataname = "bi_merge",
      bi_vars = input$`bi_vars-dataset_ADSL_singleextract-select`
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
    data_extract = list(mh_term, mh_bodsys),
    input_id = c("mh_term", "mh_bodsys"),
    merge_function = "dplyr::left_join",
    anl_name = "mh_merge"
  )

  mhist_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`mh_term-dataset_ADMH_singleextract-select`,
        "Please select MHTERM variable."
      ),
      need(
        input$`mh_bodsys-dataset_ADMH_singleextract-select`,
        "Please select MHBODSYS variable."
      )
    )

    mhist_stack <- chunks$new()
    mhist_stack_push <- function(...) {
      chunks_push(..., chunks = mhist_stack)
    }
    chunks_push_data_merge(mhist_merged_data(), chunks = mhist_stack)

    mhist_stack_push(bquote({
      mh_merge <- mh_merge[mh_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    my_calls <- template_medical_history(
      dataname = "mh_merge",
      mh_term = input$`mh_term-dataset_ADMH_singleextract-select`,
      mh_bodsys = input$`mh_bodsys-dataset_ADMH_singleextract-select`
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
    data_extract = list(v_paramcd, v_xaxis, v_aval, atirel, medname_decoding),
    input_id = c("v_paramcd", "v_xaxis", "v_aval", "atirel", "medname_decoding"),
    merge_function = "dplyr::left_join",
    anl_name = "pm_merge"
  )

  pmed_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`atirel-dataset_ADCM_singleextract-select`,
        "Please select ATIREL variable."
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

    pmed_stack_push(bquote({
      pm_merge <- pm_merge[pm_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    my_calls <- template_prior_medication(
      dataname = "pm_merge",
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
    data_extract = list(v_paramcd, v_xaxis, v_aval),
    input_id = c("v_paramcd", "v_xaxis", "v_aval"),
    merge_function = "dplyr::left_join",
    anl_name = "v_merge"
  )

  vitals_dat <- reactive({
    vitals_merged_data()$data()
  })

  output$v_paramcd_levels <- renderUI({
    paramcd_var <- input$`v_paramcd-dataset_ADVS_singleextract-select`
    paramcd_col <- vitals_dat()[[paramcd_var]]
    paramcd_col_levels <- if (is.factor(paramcd_col)) {
      levels(paramcd_col)
    }
    else {
      unique(paramcd_col)
    }

    tagList(selectInput(session$ns("v_paramcd_levels_vals"),
      "Select PARAMCD variable levels:",
      selected = paramcd_col_levels,
      choices = paramcd_col_levels, multiple = TRUE
    ))
  })

  vitals_call <- reactive({
    validate_checks()

    validate(
      need(
        input$`v_paramcd-dataset_ADVS_singleextract-select`,
        "Please select PARAMCD variable."
      ),
      need(
        input$`v_paramcd_levels_vals`,
        "Please select PARAMCD variable levels."
      ),
      need(
        input$`v_xaxis-dataset_ADVS_singleextract-select`,
        "Please select Vitals x-axis variable."
      ),
      need(
        input$`v_aval-dataset_ADVS_singleextract-select`,
        "Please select AVAL variable."
      )
    )

    vitals_stack <- chunks$new()
    vitals_stack_push <- function(...) {
      chunks_push(..., chunks = vitals_stack)
    }
    chunks_push_data_merge(vitals_merged_data(), chunks = vitals_stack)

    v_xaxis <- input$`v_xaxis-dataset_ADVS_singleextract-select`

    vitals_stack_push(bquote({
      v_merge <- v_merge[v_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    my_calls <- template_vitals(
      dataname = "v_merge",
      v_paramcd = input$`v_paramcd-dataset_ADVS_singleextract-select`,
      v_paramcd_levels = input$`v_paramcd_levels_vals`,
      v_xaxis = input$`v_xaxis-dataset_ADVS_singleextract-select`,
      v_aval = input$`v_aval-dataset_ADVS_singleextract-select`
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
    data_extract = list(atirel, medname_decoding, t_cmindc,
      t_cmdose, t_cmtrt, t_cmdosu, t_cmroute, t_cmdosfrq, t_cmstdy, t_cmendy),
    input_id = c(
      "atirel", "medname_decoding", "t_cmindc", "t_cmdose",
      "t_cmtrt", "t_cmdosu", "t_cmroute", "t_cmdosfrq", "t_cmstdy", "t_cmendy"
    ),
    merge_function = "dplyr::left_join",
    anl_name = "t_merge"
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
        input$`t_cmindc-dataset_ADCM_singleextract-select`,
        "Please select CMINDC variable."
      ),
      need(
        input$`t_cmdose-dataset_ADCM_singleextract-select`,
        "Please select CMDOSE variable."
      ),
      need(
        input$`t_cmtrt-dataset_ADCM_singleextract-select`,
        "Please select CMTRT variable."
      ),
      need(
        input$`t_cmdosu-dataset_ADCM_singleextract-select`,
        "Please select CMDOSU variable."
      ),
      need(
        input$`t_cmroute-dataset_ADCM_singleextract-select`,
        "Please select CMROUTE variable."
      ),
      need(
        input$`t_cmdosfrq-dataset_ADCM_singleextract-select`,
        "Please select CMDOSFRQ variable."
      ),
      need(
        input$`t_cmstdy-dataset_ADCM_singleextract-select`,
        "Please select CMSTDY variable."
      ),
      need(
        input$`t_cmendy-dataset_ADCM_singleextract-select`,
        "Please select CMENDY variable."
      )
    )

    therapy_stack <- chunks$new()
    therapy_stack_push <- function(...) {
      chunks_push(..., chunks = therapy_stack)
    }
    chunks_push_data_merge(therapy_merged_data(), chunks = therapy_stack)

    therapy_stack_push(bquote({
      t_merge <- t_merge[t_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    my_calls <- template_therapy(
      dataname = "t_merge",
      atirel = input$`atirel-dataset_ADCM_singleextract-select`,
      medname_decoding = input$`medname_decoding-dataset_ADCM_singleextract-select`,
      t_cmtrt = input$`t_cmtrt-dataset_ADCM_singleextract-select`,
      t_cmdosu = input$`t_cmdosu-dataset_ADCM_singleextract-select`,
      t_cmroute = input$`t_cmroute-dataset_ADCM_singleextract-select`,
      t_cmdosfrq = input$`t_cmdosfrq-dataset_ADCM_singleextract-select`,
      t_cmstdy = input$`t_cmstdy-dataset_ADCM_singleextract-select`,
      t_cmendy = input$`t_cmendy-dataset_ADCM_singleextract-select`,
      t_cmindc = input$`t_cmindc-dataset_ADCM_singleextract-select`,
      t_cmdose = input$`t_cmdose-dataset_ADCM_singleextract-select`
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
    anl_name = "ae_merge"
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

    ae_stack$push(bquote({
      ae_merge <- ae_merge[ae_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    ae_calls <- template_adverse_events(
      dataname = "ae_merge",
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
    data_extract = list(ae_term, ae_time_start, ae_time_end, ds_time_start, ds_time_end, t_cmtrt),
    input_id = c("ae_term", "ae_time_start", "ae_time_end", "ds_time_start", "ds_time_end", "t_cmtrt"),
    anl_name = "pt_merge"
  )

  patient_timeline_calls <- reactive({
    validate_checks()

    patient_timeline_stack <- chunks$new()
    chunks_push_data_merge(patient_timeline_merged_data(), chunks = patient_timeline_stack)

    patient_timeline_stack$push(bquote({
      pt_merge <- pt_merge[pt_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    patient_timeline_calls <- template_patient_timeline(
      dataname = "pt_merge",
      ae_term = input$`ae_term-dataset_ADAE_singleextract-select`,
      ae_time_start = input$`ae_time_start-dataset_ADAE_singleextract-select`,
      ae_time_end = input$`ae_time_end-dataset_ADAE_singleextract-select`,
      ds_time_start = input$`ds_time_start-dataset_ADCM_singleextract-select`,
      ds_time_end = input$`ds_time_end-dataset_ADCM_singleextract-select`,
      t_cmtrt = input$`t_cmtrt-dataset_ADCM_singleextract-select`
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
    data_extract = list(lb_timepoints, lb_aval, lb_avalu, lb_param, lb_paramcd, lb_anrind),
    input_id = c("lb_timepoints", "lb_aval", "lb_avalu", "lb_param", "lb_paramcd", "lb_anrind"),
    anl_name = "lb_merge"
  )

  labor_calls <- reactive({
    validate_checks()

    validate(
      need(
        input[[extract_input("lb_timepoints", "ADLB")]],
        "Please select timepoints variable."
      ),
      need(
        input[[extract_input("lb_aval", "ADLB")]],
        "Please select AVAL variable."
      ),
      need(
        input[[extract_input("lb_avalu", "ADLB")]],
        "Please select AVALU variable."
      ),
      need(
        input[[extract_input("lb_param", "ADLB")]],
        "Please select PARAM variable."
      ),
      need(
        input[[extract_input("lb_paramcd", "ADLB")]],
        "Please select PARAMCD variable."
      ),
      need(
        input[[extract_input("lb_anrind", "ADLB")]],
        "Please select ANRIND variable."
      )
    )

    labor_stack <- chunks$new()
    labor_stack$reset()

    chunks_push_data_merge(labor_merged_data(), chunks = labor_stack)

    labor_stack$push(bquote({
      lb_merge <- lb_merge[lb_merge[[.(patient_col)]] == .(patient_id()), ] # nolint
    }))

    labor_calls <- template_laboratory(
      dataname = "lb_merge",
      lb_timepoints = input[[extract_input("lb_timepoints", "ADLB")]],
      lb_aval = input[[extract_input("lb_aval", "ADLB")]],
      lb_avalu = input[[extract_input("lb_avalu", "ADLB")]],
      lb_param = input[[extract_input("lb_param", "ADLB")]],
      lb_paramcd = input[[extract_input("lb_paramcd", "ADLB")]],
      lb_anrind = input[[extract_input("lb_anrind", "ADLB")]]
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


  # Shinyjs wrapper ----
  encoding_list <- list(
    "Basic info" = c("bi_vars-div"),
    "Medical history" = c("mh_term-div", "mh_bodsys-div"),
    "Prior medication" = c("atirel-div", "medname_decoding-div"),
    "Vitals" = c("v_paramcd-div", "v_paramcd_levels-div", "v_xaxis-div", "v_aval-div"),
    "Therapy" = c(
      "atirel-div", "medname_decoding-div", "t_cmindc-div", "t_cmdose-div", "t_cmtrt-div",
      "t_cmdosu-div", "t_cmroute-div", "t_cmdosfrq-div", "t_cmstdy-div", "t_cmendy-div"
    ),
    "Adverse events" = c(
      "ae_term-div", "ae_tox_grade-div", "ae_causality-div",
      "ae_outcome-div", "ae_action-div", "ae_time-div", "ae_decod-div"
    ),
    "Laboratory values" = c(
      "lb_timepoints-div", "lb_aval-div", "lb_avalu-div", "lb_param-div", "lb_paramcd-div", "lb_anrind-div"
    ),
    "Patient timeline" = c(
      "ae_term-div", "ae_time_start-div", "ae_time_end-div", "ds_time_start-div", "ds_time_end-div", "t_cmtrt-div"
    )
  )

  all_encoding_list <- c(
    "bi_vars-div", "mh_term-div", "mh_bodsys-div", "v_paramcd-div", "v_xaxis-div", "v_aval-div",
    "atirel-div", "medname_decoding-div", "Vitals-div", "v_paramcd-div", "v_paramcd_levels-div",
    "v_xaxis-div", "vitals_ava-divl", "medname_decoding-div", "t_cmindc-div", "t_cmdose-div", "t_cmtrt-div",
    "t_cmdosu-div", "t_cmroute-div", "t_cmdosfrq-div", "t_cmstdy-div", "t_cmendy-div", "ae_tox_grade-div",
    "ae_outcome-div", "ae_action-div", "ae_time-div", "ae_decod-div", "ae_causality-div", "lb_timepoints-div",
    "lb_aval-div", "lb_avalu-div", "lb_param-div", "lb_paramcd-div", "lb_anrind-div", "ae_term-div",
    "ae_time_start-div", "ae_time_end-div", "ds_time_start-div", "ds_time_end-div"
  )

  observeEvent(input$tabs, handlerExpr = {
    lapply(encoding_list[[input$tabs]], shinyjs::show)
    lapply(setdiff(all_encoding_list, encoding_list[[input$tabs]]), shinyjs::hide)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(
      bi_vars,
      mh_term,
      v_aval,
      v_paramcd,
      v_param,
      v_xaxis,
      lb_aval,
      lb_paramcd,
      lb_param,
      lb_anrind,
      lb_timepoints,
      atirel,
      medname_decoding,
      t_cmindc,
      t_cmdose,
      t_cmtrt,
      t_cmdosu,
      t_cmroute,
      t_cmdosfrq,
      t_cmstdy,
      t_cmendy,
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
