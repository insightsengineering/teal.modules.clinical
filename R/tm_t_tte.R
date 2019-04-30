#' Time To Event Table Teal Module
#'
#' Time to event table as defined in \code{\link[tern]{t_tte}} in the
#' \code{tern} package
#'
#' @inheritParams teal::standard_layout
#' @param label menue item label of the module in the teal app
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm_var}
#' @param arm_ref_comp (\code{\link[teal]{choices_selected}}) optional, if specified it must be a named list with each
#'   element corresponding to an arm variable in \code{asl} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd \code{\link[teal]{choices_selected}} object with all available choices and preselected option for
#' variable names that can be used as \code{PARAMCD} variable
#' @param strata_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for stratification
#' @param time_points \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used \code{\link[tern]{t_tte}}
#' @param time_unit (\code{character}) with unit of \code{dataname$AVAL}, please use singular e.g. month instead
#'   of months
#' @param event_desc_var (\code{character}) variable name with the event description information,
#'   optional
#'
#' @details
#' This modules expects that the analysis data has the following variables
#'
#' \tabular{ll}{
#'  \code{AVAL} \tab time to event\cr
#'  \code{CNSR} \tab boolean or 0,1 is element in \code{AVAL} censored\cr
#'  \code{PARAMCD} \tab variable used to filter for endpoint (e.g. OS), after
#'  filtering for \code{paramcd} one observation per patient is expected
#' }
#'
#' The arm variables, stratification variables and taken from the \code{asl}
#' data.
#'
#'
#' @template author_waddella
#'
#' @export
#' @import magrittr
#' @importFrom forcats fct_collapse fct_relevel
#'
#' @examples
#'
#' asl <- random.cdisc.data::radsl(seed = 1)
#' ate <- random.cdisc.data::radtte(asl, seed = 1)
#'
#' keys(asl) <- keys(ate) <- c("USUBJID", "STUDYID")
#'
#' #<code
#' app <- teal::init(
#'     data = cdisc_data(ASL = asl, ATE = ate,
#'         code = "
#'             asl <- random.cdisc.data::radsl(seed = 1)
#'             ate <- random.cdisc.data::radtte(asl, seed = 1)
#'             keys(asl) <- keys(ate) <- c('USUBJID', 'STUDYID')",
#'         check = TRUE),
#'     modules = root_modules(
#'         tm_t_tte(
#'             label = "Time To Event Table",
#'             dataname = 'ATE',
#'             arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'             paramcd = choices_selected(unique(ate$PARAMCD), "OS"),
#'             strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX"),
#'             time_points = choices_selected(c(6, 8), 6),
#'             time_unit = "month",
#'             event_desc_var = "EVNTDESC"
#'         )
#'     )
#' )
#'
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' ## Define default reference & comparison arms based on
#' ## ARM variable
#' library(magrittr)
#' library(dplyr)
#'
#' asl <- dplyr::mutate(random.cdisc.data::radsl(seed = 1),
#'   ARM1 = sample(c("DUMMY A", "DUMMY B"),
#'   dplyr::n(), TRUE))
#' ate <- random.cdisc.data::radtte(asl, seed = 1)
#' keys(asl) <- keys(ate) <- c("USUBJID", "STUDYID")
#'
#' arm_ref_comp = list(
#'   ACTARMCD = list(
#'     ref = "ARM A",
#'     comp = c("ARM B", "ARM C")
#'   ),
#'   ARM1 = list(
#'     ref = "DUMMY B",
#'     comp = "DUMMY A"
#'   )
#' )
#' app <- teal::init(
#'     data = cdisc_data(ASL = asl, ATE = ate,
#'         code = "library(dplyr)
#'    asl <- random.cdisc.data::radsl(seed = 1) %>%
#' dplyr::mutate(., ARM1 = sample(c('DUMMY A', 'DUMMY B'), n(), TRUE))
#'             ate <- random.cdisc.data::radtte(asl, seed = 1)
#'             keys(asl) <- keys(ate) <- c('USUBJID', 'STUDYID')",
#'         check = TRUE),
#'     modules = root_modules(
#'         tm_t_tte(
#'          label = "Time To Event Table",
#'          dataname = 'ATE',
#'          arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'          arm_ref_comp = arm_ref_comp,
#'          paramcd = choices_selected(unique(ate$PARAMCD), "OS"),
#'          strata_var = choices_selected(c("SEX", "MLIVER"), "SEX"),
#'          time_points = choices_selected(c(6, 8), 6),
#'          time_unit = "months",
#'          event_desc_var = "EVNTDESC"
#'         )
#'     )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_tte <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     time_points,
                     time_unit = "months",
                     event_desc_var = NULL,
                     pre_output = NULL,
                     post_output = NULL
                     ) {

  stopifnot(length(dataname) == 1)
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(strata_var))
  stopifnot(is.choices_selected(time_points))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_tte,
    ui = ui_t_tte,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp,
      time_unit = time_unit,
      event_desc_var = event_desc_var,
      label = label
    ),
    filters = dataname
  )
}

#' @import teal.devel
ui_t_tte <- function(id, ...) {

  a <- list(...) # module args

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("tte_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("paramcd"),
                          "Select Endpoint",
                          a$paramcd$choices,
                          a$paramcd$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE),
      selectInput(ns("ref_arm"),
                  "Reference Group",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      helpText("Multiple reference groups are automatically combined into a single group."),
      selectInput(ns("comp_arm"),
                  "Comparison Group",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      checkboxInput(ns("combine_comp_arms"),
                    "Combine all comparison groups?",
                    value = FALSE),
      optionalSelectInput(ns("strata_var"),
                          "Stratify by",
                          a$strata_var$choices,
                          a$strata_var$selected,
                          multiple = TRUE,
                          label_help = helpText("from ", tags$code("ASL"))),
      optionalSelectInput(ns("time_points"),
                          "Time Points",
                          a$time_points$choices,
                          a$time_points$selected,
                          multiple = TRUE),
      if (!is.null(a$event_desc_var)) {
        helpText("Event Description Variable: ", tags$code(a$event_desc_var))
      }
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @import teal.devel
#' @importFrom rtables as_html
srv_t_tte <- function(input, output, session, datasets, dataname,
                      arm_ref_comp, time_unit, event_desc_var,
                      label) {

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte"
  )

  use_chunks(session)

  # Create output

  table_reactive <- reactive({
    # resolve all reactive expressions
    # nolint start
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    # nolint end

    paramcd <- input$paramcd # nolint
    strata_var <- input$strata_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    time_points <- as.numeric(input$time_points)

    if (length(strata_var) == 0) strata_var <- NULL

    time_points <- if (length(time_points) == 0) NULL else sort(time_points)

    # validate your input values
    validate_standard_inputs(
      asl = ASL_FILTERED, # nolint
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
      anl = ANL_FILTERED, # nolint
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "CNSR", event_desc_var),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate(need(is.logical(combine_comp_arms), "need combine arm information"))

    # do analysis

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED) # nolint

    # Delete chunks that are used for reproducible code
    renew_chunk_environment(envir = environment())
    renew_chunks()

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var)) #nolint
    anl_vars <- unique(c("USUBJID", "STUDYID", "AVAL", "CNSR", event_desc_var)) #nolint

    ## Now comes the analysis code
    set_chunk(expression = bquote(ref_arm <- .(ref_arm)))
    set_chunk(expression = bquote(comp_arm <- .(comp_arm)))
    set_chunk(expression = bquote(strata_var <- .(strata_var)))
    set_chunk(expression = bquote(combine_comp_arms <- .(combine_comp_arms)))

    set_chunk(expression = bquote(asl_p <- subset(ASL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))))# nolint
    set_chunk(expression = bquote(anl_endpoint <- subset(.(as.name(anl_name)), PARAMCD == .(paramcd))))

    set_chunk(expression = bquote(anl <- merge(
        x = asl_p[, .(asl_vars)],
        y = anl_endpoint[, .(anl_vars)],
        all.x = FALSE, all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )))

    set_chunk(expression = bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])))
    set_chunk(expression = bquote(arm <- combine_levels(arm, ref_arm)))
    if (combine_comp_arms) {
      set_chunk(expression = bquote(arm <- combine_levels(arm, comp_arm)))
    }
    set_chunk(expression = bquote(anl[[.(arm_var)]] <- droplevels(arm)))

    eval_remaining()

    validate(need(nrow(get_envir_chunks()$anl) > 15, "need at least 15 data points"))

    set_chunk(
        id = "final_table",
        expression =
            call(
                name = "t_tte",
                formula = as.formula(paste0(
                        "Surv(AVAL, !CNSR) ~ arm(", arm_var, ")",
                        if (length(strata_var) == 0){
                              ""
                            }else{
                              paste0(" + strata(", paste(strata_var, collapse = ", "), ")")
                            }
                    )),
                data = quote(anl),
                event_descr = if (is.null(event_desc_var)) NULL else call("as.factor", as.name(event_desc_var)),
                time_points = time_points,
                time_unit = time_unit
            )
    )
  })

  output$tte_table <- renderUI({
        table_reactive()

        table_result <- eval_remaining()
        validate(need(is(table_result, "rtable"), "Evaluation with tern t_tte failed."))

        as_html(table_result)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
        title = "Cross Table",
        rcode = get_rcode(
            datasets = datasets,
            dataname = c("ASL", dataname),
            title = label
        )
    )
  })

}
