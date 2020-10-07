#' Forest Response Plot teal module
#'
#' This is teal module produces a grid style Forest plot for response data with ADaM structure
#'
#' @inheritParams tm_t_tte
#' @inheritParams shared_params
#' @param subgroup_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as the default subgroups
#' @param conf_level \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for confidence level, each within range of (0, 1).
#' @param fixed_symbol_size (\code{logical}) When (\code{TRUE}), the same symbol size is used for plotting each
#' estimate. Otherwise, the symbol size will be proportional to the sample size in each each subgroup.
#' @param cex (\code{numeric}) multiplier applied to overall font size
#'
#' @export
#' @importFrom rtables var_labels "var_labels<-"
#'
#' @template author_song24
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(cached = TRUE) %>%
#'      dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- radsl(cached = TRUE)'),
#'     cdisc_dataset("ADRS", ADRS,
#'       code = 'ADRS <- radrs(cached = TRUE) %>%
#'               dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET"))'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       paramcd = choices_selected(c("BESRSPI", "INVET"), "BESRSPI"),
#'       subgroup_var = choices_selected(names(ADSL), c("BMRKR2", "SEX")),
#'       strata_var = choices_selected(c("STRATA1", "STRATA2"), "STRATA2"),
#'       plot_height = c(600L, 200L, 2000L)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_forest_rsp <- function(label,
                            dataname,
                            arm_var,
                            paramcd,
                            subgroup_var,
                            strata_var,
                            conf_level = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995), 0.95, keep_order = TRUE), # nolint
                            fixed_symbol_size = TRUE,
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(980L, 500L, 2000L),
                            cex = 1.3,
                            pre_output = NULL,
                            post_output = NULL) {

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))
  stopifnot(is.choices_selected(strata_var))
  stopifnot(is.choices_selected(conf_level))
  stopifnot(is_logical_single(fixed_symbol_size))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)
  stop_if_not(list(is_numeric_single(cex), "cex should be single numeric object))"))
  stop_if_not(list(
    is.null(pre_output) || is(pre_output, "shiny.tag"),
    "pre_output should be either null or shiny.tag type of object"
  ))
  stop_if_not(list(
    is.null(pre_output) || is(pre_output, "shiny.tag"),
    "pre_output should be either null or shiny.tag type of object"
  ))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_forest_rsp,
    ui = ui_g_forest_rsp,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      cex = cex,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    filters = dataname
  )
}


ui_g_forest_rsp <- function(id, ...) {

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
        label_help = helpText("Select one type of response to analyze.")
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
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
        multiple = TRUE
      ),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(
        ns("comp_arm"),
        "Comparison Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
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
      tags$label(
        "Plot Settings",
        class = "text-primary",
        style = "margin-top: 15px;"
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
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_forest_rsp <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             cex,
                             plot_height,
                             plot_width) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    datasets = datasets,
    arm_ref_comp = NULL,
    module = "tm_g_forest_rsp"
  )


  # Update UI choices depending on selection of previous options
  observe({
    paramcd <- input$paramcd
    anl <- datasets$get_data(dataname, filtered = FALSE)
    rsp_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(
      session, "responders",
      choices = rsp_choices,
      selected = intersect(rsp_choices, c("CR", "PR"))
    )
  })


  plot_r <- reactive({

    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    var_labels(adsl_filtered) <- var_labels(
      datasets$get_data("ADSL", filtered = FALSE)
    )

    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    var_labels(anl_filtered) <- var_labels(
      datasets$get_data(dataname, filtered = FALSE)
      )

    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var
    strata_var <- input$strata_var
    conf_level <- as.numeric(input$conf_level)
    col_symbol_size <- if (input$fixed_symbol_size) {
      NULL
    } else {
      1
    }

    # validate your input values
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var, strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate_in(responders, anl_filtered$AVALC, "Responder values cannot be found in AVALC.")
    validate_in(paramcd, anl_filtered$PARAMCD, "Response parameter cannot be found in PARAMCD.")
    validate(
      need(length(conf_level) == 1, "Please select level of confidence.")
    )
    if (!is.null(subgroup_var)){
      need(all(vapply(adsl_filtered[, subgroup_var], is.factor, logical(1))),
           "Not all subgroup variables are factors.")
    }

    # perform analysis
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var, strata_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVALC") # nolint

    chunks_reset(envir = environment())

    chunks_push(bquote({
      adsl_p <- subset(.(as.name(adsl_name)), .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm)))
      anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))
    }))

    if (!is.null(subgroup_var)) {
      chunks_push(bquote({
        adsl_p[, .(subgroup_var)] <- droplevels(adsl_p[, .(subgroup_var)])
      }))
    }

    chunks_push(bquote({
      anl <- merge(
        adsl_p[, .(adsl_vars)],
        anl_p[, .(anl_vars)],
        all.x = FALSE,
        all.y = FALSE,
        by = c("USUBJID", "STUDYID"))

      arm <- relevel(as.factor(anl[[.(arm_var)]]), .(ref_arm)[1])

      arm <- combine_levels(arm, .(ref_arm))
      arm <- combine_levels(arm, .(comp_arm))

      anl[[.(arm_var)]] <- droplevels(arm)

      levels(anl[[.(arm_var)]]) <- sapply(
        levels(anl[[.(arm_var)]]),
        function(x) {
          paste(strwrap(x, width = 15), collapse = "\n")
        }
      )

      var_labels(anl) <- .(
        c(
          var_labels(adsl_filtered[adsl_vars]),
          var_labels(anl_filtered[c("AVALC")])
        )
      )

    }))

    chunks_safe_eval()
    anl <- chunks_get_var("anl")

    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))

    chunks_push(bquote({
      tbl <- t_forest_rsp(
        rsp = anl$AVALC %in% .(responders),
        col_by = anl[[.(arm_var)]],
        row_by_list = .(if (length(subgroup_var) > 0) {
          bquote(anl[, .(subgroup_var), drop = FALSE])
        } else {
          bquote(NULL)
        }),
        strata_data = if (!is.null(.(strata_var))) {
          anl[, .(strata_var), drop = FALSE]
        } else {
          NULL
        },
        conf_level = .(conf_level),
        total = "All Patients",
        dense_header = TRUE
      )

      row.names(tbl) <- sapply(
        row.names(tbl),
        function(x) {
          paste(strwrap(x, width = 20), collapse = "\n")
        }
      )
    }))

    chunks_safe_eval()

    chunks_push(bquote({
      p <- g_forest(
        tbl = tbl,
        col_x = 8,
        col_ci = 9,
        vline = 1,
        forest_header = paste0(levels(anl[[.(arm_var)]]), "\nbetter"),
        xlim = c(.1, 10),
        logx = TRUE,
        x_at = c(.1, 1, 10),
        draw = FALSE,
        col_symbol_size = .(col_symbol_size)
      )
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(p, title = "Forest plot", footnotes = footnotes(p),
                           gp_footnotes = gpar(fontsize = 12))
      }
      grid.newpage()
      grid.draw(p)
    }))


    chunks_safe_eval()

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
      title = "R Code for the Current Reponse Forest Plot",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = "Response Forest Plot"
      )
    )
  })

}
