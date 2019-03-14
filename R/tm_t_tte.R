#' Time To Event Table Teal Module
#'
#' Time to event table as defined in \code{\link[tern]{t_tte}} in the
#' \code{tern} package
#'
#' @inheritParams teal::standard_layout
#' @param label menue item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm_var}
#' @param arm_ref_comp optional, if specified it must be a named list with each
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
#' @param time_unit string with unit of \code{dataname$AVAL}
#' @param event_desrc_var variable name with the event description information,
#'   optional
#' @param code_data_processing string with data preprocessing before the teal
#'   app is initialized
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
#'
#' @importFrom forcats fct_collapse fct_relevel
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' asl <- radsl(seed = 1)
#' ate <- radtte(asl, seed = 1)
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' attr(ate, "source") <- "random.cdisc.data::radtte(asl, seed = 1)"
#'
#' x <- teal::init(
#'   data = list(ASL = asl, ATE = ate),
#'   modules = root_modules(
#'     tm_t_tte(
#'        label = "Time To Event Table",
#'        dataname = 'ATE',
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        paramcd = choices_selected(unique(ate$PARAMCD), "OS"),
#'        strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX"),
#'        time_points = choices_selected(c(6, 8), 6),
#'        time_unit = "months",
#'        event_desrc_var = "EVNTDESC"
#'     )
#'   )
#' )
#'
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
#'
#' ## Define default reference & comparison arms based on
#' ## ARM variable
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' asl <- radsl(seed = 1) %>%
#'   mutate(., ARM1 = sample(c("DUMMY A", "DUMMY B"), n(), TRUE))
#' ate <- radtte(asl, seed = 1)
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1) %>%
#'   mutate(., ARM1 = sample(c('DUMMY A', 'DUMMY B'), n(), TRUE))"
#' attr(ate, "source") <- "random.cdisc.data::radtte(asl, seed = 1)"
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
#'
#' x <- teal::init(
#'   data = list(ASL = asl, ATE = ate),
#'   modules = root_modules(
#'     tm_t_tte(
#'        label = "Time To Event Table",
#'        dataname = 'ATE',
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        arm_ref_comp = arm_ref_comp,
#'        paramcd = choices_selected(unique(ate$PARAMCD), "OS"),
#'        strata_var = choices_selected(c("SEX", "MLIVER"), "SEX"),
#'        time_points = choices_selected(c(6, 8), 6),
#'        time_unit = "months",
#'        event_desrc_var = "EVNTDESC"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_t_tte <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     time_points,
                     time_unit = "months",
                     event_desrc_var = NULL,
                     pre_output = NULL,
                     post_output = NULL,
                     code_data_processing = NULL) {

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
      event_desrc_var = event_desrc_var,
      code_data_processing = code_data_processing
    ),
    filters = dataname
  )
}


ui_t_tte <- function(id, ...) {

  a <- list(...) # module args

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(uiOutput(ns("tte_table"))),
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
                          multiple = TRUE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_t_tte <- function(input, output, session, datasets, dataname,
                      arm_ref_comp, time_unit, event_desrc_var,
                      code_data_processing) {

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  teal.devel::arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte"
  )

  chunks <- list(
    vars = "# No Calculated",
    data = "# No Calculated",
    t_tte = "# No Calculated"
  )

  # Create output
  output$tte_table <- renderUI({

    # resolve all reactive expressions
    asl_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd # nolint
    strata_var <- input$strata_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    time_points <- as.numeric(input$time_points)

    if (length(strata_var) == 0) strata_var <- NULL

    time_points <- if (length(time_points) == 0) NULL else sort(time_points)

    # Delete chunks that are used for reproducible code
    for (i in seq_along(chunks)) chunks[[i]] <<- "# Not calculated"

    # validate your input values
    teal.devel::validate_standard_inputs(
      asl = asl_filtered,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "CNSR", event_desrc_var),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate(need(is.logical(combine_comp_arms), "need combine arm information"))


    # do analysis

    anl_name <- paste0(dataname, "_filtered")
    assign(anl_name, anl_filtered) # so that we can refer to the 'correct' data name

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var)) # nolint
    anl_vars <- unique(c("USUBJID", "STUDYID", "AVAL", "CNSR", event_desrc_var)) # nolint

    ## Now comes the analysis code
    chunks$vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
      strata_var <- .(strata_var)
      combine_comp_arms <- .(combine_comp_arms)
    })

    chunks$data <<- bquote({
      asl_p <- subset(asl_filtered, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))

      anl_endpoint <- subset(.(as.name(anl_name)), PARAMCD == .(paramcd))

      anl <- merge(
        x = asl_p[, .(asl_vars)],
        y = anl_endpoint[, .(anl_vars)],
        all.x = FALSE, all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )

      arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])

      arm <- combine_levels(arm, ref_arm)
      if (combine_comp_arms) {
        arm <- combine_levels(arm, comp_arm)
      }

      anl[[.(arm_var)]] <- droplevels(arm)

    })

    eval(chunks$data)
    validate(need(nrow(anl) > 15, "need at least 15 data points"))


    chunks$t_tte <<- call(
      name = "t_tte",
      formula = as.formula(paste0(
        "Surv(AVAL, !CNSR) ~ arm(", arm_var, ")",
        if (length(strata_var) == 0) "" else paste0(" + strata(", paste(strata_var, collapse = ", "), ")")
      )),
      data = quote(anl),
      event_descr = if (is.null(event_desrc_var)) NULL else call("as.factor", as.name(event_desrc_var)),
      time_points = time_points,
      time_unit = time_unit
    )

    tbl <- try(eval(chunks$t_tte))

    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate time to event table:\n\n", tbl)))

    rtables::as_html(tbl)
  })


  observeEvent(input$show_rcode, {

    header <- teal.devel::get_rcode_header(
      title = "Time To Event Table",
      datanames = if (is.null(code_data_processing)) dataname else datasets$datanames(),
      datasets = datasets,
      code_data_processing
    )

    str_rcode <- paste(c(
      "",
      header,
      "",
      teal.devel::remove_enclosing_curly_braces(deparse(chunks$vars)),
      "",
      teal.devel::remove_enclosing_curly_braces(deparse(chunks$data)),
      "",
      deparse(chunks$t_tte)
    ), collapse = "\n")

    showModal(modalDialog(
      title = "R Code for the Current Time To Event Table",
      tags$pre(tags$code(class = "R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}
