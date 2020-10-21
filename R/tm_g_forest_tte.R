#' Forest Survival Plot teal Module
#'
#' This is teal module produces a grid style Forest plot for time-to-event data
#' with ADaM structure
#'
#' @inheritParams tm_g_forest_rsp
#'
#' @export
#' @importFrom rtables var_labels "var_labels<-"
#'
#' @template author_song24
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' ADSL$RACE <- droplevels(ADSL$RACE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL,
#'       code = 'ADSL <- radsl(cached = TRUE)
#'               ADSL$RACE <- droplevels(ADSL$RACE)'),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- radtte(cached = TRUE)'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_forest_tte(
#'        label = "Forest Survival",
#'        dataname = "ADTTE",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        subgroup_var = choices_selected(names(ADSL), c("RACE", "SEX", "BMRKR2")),
#'        paramcd = choices_selected(c("OS", "PFS"), "OS"),
#'        strata_var = choices_selected(c("STRATA1", "STRATA2"), "STRATA2"),
#'        plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_forest_tte <- function(label,
                            dataname,
                            arm_var,
                            subgroup_var,
                            paramcd,
                            strata_var,
                            conf_level = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995), 0.95, keep_order = TRUE), # nolint
                            fixed_symbol_size = TRUE,
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(980L, 500L, 2000L),
                            cex = 1.3,
                            pre_output = NULL,
                            post_output = NULL) {

  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))
  stopifnot(is.choices_selected(strata_var))
  stopifnot(is.choices_selected(conf_level))
  stopifnot(is_logical_single(fixed_symbol_size))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      cex = cex,
      plot_height = plot_height,
      plot_width = plot_width),
    filters = dataname
  )
}


ui_g_forest_tte <- function(id, ...) {

  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = plot_with_settings_ui(id = ns("myplot"), height = a$plot_height, width = a$plot_width),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "PARAMCD",
        a$paramcd$choices,
        a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed,
        label_help = helpText("Select an endpoint to analyze.")
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE,
        fixed = a$arm_var$fixed
      ),
      selectInput(
        ns("ref_arm"),
        "Reference Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(
        ns("comp_arm"),
        "Comparison Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      optionalSelectInput(
        ns("subgroup_var"),
        "Subgroup Variables",
        a$subgroup_var$choices,
        a$subgroup_var$selected,
        multiple = TRUE,
        label_help = helpText("are taken from", tags$code("ADSL")),
        fixed = a$subgroup_var$fixed
      ),
      optionalSelectInput(ns("strata_var"),
                          "Stratify by",
                          a$strata_var$choices,
                          a$strata_var$selected,
                          multiple = TRUE,
                          label_help = helpText("from ", tags$code("ADSL")),
                          fixed = a$strata_var$fixed
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE)
        )
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_forest_tte <- function(input, output, session, datasets, dataname, plot_height, plot_width, cex = 1.5) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var", # from UI
    datasets = datasets,
    arm_ref_comp = NULL,
    module = "tm_g_forest_tte"
  )


  plot_r <- reactive({

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) #nolint
    var_labels(ADSL_FILTERED) <- var_labels(
      datasets$get_data("ADSL", filtered = FALSE)
    )

    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) #nolint
    var_labels(ANL_FILTERED) <- var_labels(
      datasets$get_data(dataname, filtered = FALSE)
      )

    paramcd <- input$paramcd
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var
    strata_var <- input$strata_var

    if (length(strata_var) == 0) {
      strata_var <- NULL
    }
    conf_level <- as.numeric(input$conf_level)
    col_symbol_size <- if (input$fixed_symbol_size) {
      NULL
    } else {
      1
    }
    # validate your input values
    validate_standard_inputs(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var, strata_var),
      anl = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALU", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate_in(paramcd, ANL_FILTERED$PARAMCD, "Time-to-Event Endpoint cannot be found in PARAMCD")
    validate(
      need(length(conf_level) == 1, "Please select level of confidence.")
    )
    if (!is.null(subgroup_var)){
      need(all(vapply(ADSL_FILTERED[, subgroup_var], is.factor, logical(1))),
           "Not all subgroup variables are factors.")
    }

    anl_data_name <- paste0(dataname, "_FILTERED")
    assign(anl_data_name, ANL_FILTERED)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var, strata_var)) #nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALU", "CNSR") #nolint

    chunks_push(bquote({

      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
      strata_var <- .(strata_var)
      adsl_p <- subset(ADSL_FILTERED, ADSL_FILTERED[[.(arm_var)]] %in% c(ref_arm, comp_arm))

    }))

    if (!is.null(subgroup_var)) {
      chunks_push(bquote({
        adsl_p[, .(subgroup_var)] <- droplevels(adsl_p[, .(subgroup_var)])
      }))
    }

    chunks_push(bquote({

      anl_p <- subset(.(as.name(anl_data_name)), PARAMCD %in% .(paramcd))

      anl <- merge(
        adsl_p[, .(adsl_vars)],
        anl_p[, .(anl_vars)],
        all.x = FALSE,
        all.y = FALSE,
        by = c("USUBJID", "STUDYID"))
      arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])
      arm <- combine_levels(arm, ref_arm)
      arm <- combine_levels(arm, comp_arm)

      anl[[.(arm_var)]] <- droplevels(arm)

      levels(anl[[.(arm_var)]]) <- sapply(
        levels(anl[[.(arm_var)]]),
        function(x) paste(strwrap(x, width = 15), collapse = "\n")
      )

      var_labels(anl) <- .(
        c(
          var_labels(ADSL_FILTERED[adsl_vars]),
          var_labels(ANL_FILTERED[c("AVAL", "AVALU", "CNSR")])
        )
      )

    }))

    chunks_safe_eval()
    anl <- chunks_get_var("anl")
    validate(need(nrow(anl) > 15, "need at least 15 data points"))

    chunks_push(bquote({
      tbl <- t_forest_tte(
        tte = anl$AVAL,
        is_event = anl$CNSR == 0,
        col_by = anl[[.(arm_var)]],
        time_unit = tolower(anl$AVALU[1]),
        row_by_list = .(if (length(subgroup_var) > 0) {
            bquote(anl[, .(subgroup_var), drop = FALSE])
          } else {
            bquote(NULL)
          }),
        strata_data = .(if (!is.null(strata_var)) {
          bquote(anl[, .(strata_var), drop = FALSE])
        } else {
          bquote(NULL)
        }),
        total = "All Patients",
        conf_level = .(conf_level),
        dense_header = TRUE
      )

      row.names(tbl) <- sapply(row.names(tbl), function(x) paste(strwrap(x, width = 20), collapse = "\n"))

    }))

    chunks_safe_eval()

    chunks_push(bquote({

      p <- g_forest(
        tbl = tbl,
        col_x = 8,
        col_ci = 9,
        vline = 1,
        forest_header =  paste0(rev(levels(anl[[.(arm_var)]])), "\nbetter"),
        xlim = c(.1, 10),
        logx = TRUE,
        x_at = c(.1, 1, 10),
        col_symbol_size = .(col_symbol_size),
        draw = FALSE)
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(p, title = "Forest plot", footnotes = footnotes(p),
                           gp_footnotes = gpar(fontsize = 12))
      }
      grid.newpage()
      grid.draw(p)

    }))

    chunks_safe_eval()

    chunks_get_var("p")
  })

  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for the Current Time-to-Event Forest Plot",
      rcode = get_rcode(
        datasets = datasets,
        datanames = dataname,
        title = "Time-to-Event Forest Plot"
      )
    )
  })
}
