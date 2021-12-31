#' Template: Laboratory test results with highest grade post-baseline
#' @inheritParams template_arguments
#' @param atoxgr_var (`character`)\cr the variable name indicating
#' Analysis Toxicity Grade.
#' @param worst_high_flag_var (`character`)\cr the variable name indicating
#' Worst High Grade flag
#' @param worst_low_flag_var (`character`)\cr the variable name indicating
#' Worst Low Grade flag
#' @param worst_flag_indicator (`character`)\cr value indicating worst grade.
#'
#' @seealso [tm_t_abnormality_by_worst_grade()]
template_abnormality_by_worst_grade <- function(parentname, # nolint
                                                dataname,
                                                arm_var,
                                                id_var = "USUBJID",
                                                paramcd = "PARAMCD",
                                                atoxgr_var = "ATOXGR",
                                                worst_high_flag_var = "WGRHIFL",
                                                worst_low_flag_var = "WGRLOFL",
                                                worst_flag_indicator = "Y",
                                                add_total = FALSE,
                                                drop_arm_levels = TRUE,
                                                basic_table_args = teal.devel::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(id_var),
    assertthat::is.string(paramcd),
    assertthat::is.string(atoxgr_var),
    assertthat::is.string(worst_high_flag_var),
    assertthat::is.string(worst_low_flag_var),
    assertthat::is.string(worst_flag_indicator),
    assertthat::is.flag(add_total),
    assertthat::is.flag(drop_arm_levels)
  )

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl_labels <- rtables::var_labels(df),
      env = list(
        df = as.name(dataname)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        dplyr::mutate(
          # Changed the following prepo step methodology as not
          # all cases have grade = 4 (realized with nsdl real data)
          GRADE_DIR = factor(
            case_when(
              as.numeric(as.character(atoxgr_var)) < 0 ~ "LOW",
              atoxgr_var == "0" ~ "ZERO",
              as.numeric(as.character(atoxgr_var)) > 0 ~ "HIGH"
            ),
            levels = c("LOW", "ZERO", "HIGH")
          ),
          # Changed the following prepo step methodology as not
          # all cases have grade = 4 (realized with nsdl real data)
          GRADE_ANL = factor(
            abs(
              as.numeric(
                as.character(atoxgr_var)
              )
            )
          )
        ) %>%
        dplyr::filter(worst_low_flag_var == worst_flag_indicator | worst_high_flag_var == worst_flag_indicator) %>%
        droplevels(),
      env = list(
        df = as.name(dataname),
        worst_low_flag_var = as.name(worst_low_flag_var),
        worst_high_flag_var = as.name(worst_high_flag_var),
        worst_flag_indicator = worst_flag_indicator,
        atoxgr_var = as.name(atoxgr_var)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    quote(
      expr = rtables::var_labels(anl) <- c(anl_labels, "Direction of Abnormality", "Highest Grade")
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

  y$data <- bracket_expr(data_list)

  # map creation

  prep_list <- list()

  prep_list <- add_expr(
    prep_list,
    substitute(
      expr = map <- unique(
        anl[anl[["GRADE_DIR"]] != "ZERO", c(paramcd, "GRADE_DIR", "GRADE_ANL")]
      ) %>%
        lapply(as.character) %>%
        as.data.frame() %>%
        dplyr::arrange(paramcd, desc(GRADE_DIR), GRADE_ANL),
      env = list(
        paramcd = paramcd
      )
    )
  )

  y$layout_prep <- bracket_expr(prep_list)

  parsed_basic_table_args <- teal.devel::parse_basic_table_args(
    teal.devel::resolve_basic_table_args(user_table = basic_table_args)
  )

  # layout start
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ) %>%
          rtables::add_colcounts(),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    } else {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(var = arm_var) %>%
          rtables::add_colcounts(),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    }
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_rows_by(
        paramcd,
        label_pos = "topleft",
        split_label = obj_label(anl[[paramcd]])
      ) %>%
        summarize_num_patients(
          var = id_var,
          required = "GRADE_ANL",
          .stats = "unique_count"
        ) %>%
        rtables::split_rows_by(
          "GRADE_DIR",
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map),
          split_label = obj_label(anl$GRADE_DIR)
        ) %>%
        count_abnormal_by_worst_grade(
          var = "GRADE_ANL",
          variables = list(id = id_var, param = paramcd, grade_dir = "GRADE_DIR")
        ) %>%
        rtables::append_topleft("    Highest Grade"),
      env = list(
        paramcd = paramcd,
        id_var = id_var
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' Teal Module: Laboratory test results with highest grade post-baseline
#'
#' @inheritParams module_arguments
#' @inheritParams template_abnormality_by_worst_grade
#' @param atoxgr_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#' object with all available choices and preselected option
#' for variable names that can be used as Analysis Toxicity Grade.
#' @param worst_high_flag_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr object with all available
#' choices and preselected option for variable names that can be used as Worst High Grade flag.
#' @param worst_low_flag_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr object with all available
#' choices and preselected option for variable names that can be used as Worst Low Grade flag.
#' @param worst_flag_indicator ([teal::choices_selected()] or [teal::data_extract_spec()])\cr value indicating
#' worst grade.
#' @seealso [template_abnormality_by_worst_grade()]
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adlb <- synthetic_cdisc_data("latest")$adlb %>%
#'   filter(!AVISIT %in% c("SCREENING", "BASELINE"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADLB", adlb,
#'       code = "ADLB <- synthetic_cdisc_data('latest')$adlb %>%
#'         filter(!AVISIT %in% c('SCREENING', 'BASELINE'))"
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_abnormality_by_worst_grade(
#'       label = "Laboratory test results with highest grade post-baseline",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adlb, "PARAMCD", "PARAM"),
#'         selected = c("ALT", "CRP", "IGA")
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'   filter = (
#'     list(
#'       ADSL = list(SAFFL = "Y"),
#'       ADLB = list(ONTRTFL = "Y")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality_by_worst_grade <- function(label, # nolint
                                            dataname,
                                            parentname = ifelse(
                                              inherits(arm_var, "data_extract_spec"),
                                              teal.devel::datanames_input(arm_var),
                                              "ADSL"
                                            ),
                                            arm_var,
                                            id_var = choices_selected(
                                              variable_choices(
                                                dataname,
                                                subset = "USUBJID"
                                              ),
                                              selected = "USUBJID", fixed = TRUE
                                            ),
                                            paramcd,
                                            atoxgr_var = choices_selected(
                                              variable_choices(
                                                dataname,
                                                subset = "ATOXGR"
                                              ),
                                              selected = "ATOXGR", fixed = TRUE
                                            ),
                                            worst_high_flag_var = choices_selected(
                                              variable_choices(
                                                dataname,
                                                subset = "WGRHIFL"
                                              ),
                                              selected = "WGRHIFL", fixed = TRUE
                                            ),
                                            worst_low_flag_var = choices_selected(
                                              variable_choices(
                                                dataname,
                                                subset = "WGRLOFL"
                                              ),
                                              selected = "WGRLOFL", fixed = TRUE
                                            ),
                                            worst_flag_indicator = choices_selected(
                                              value_choices(
                                                dataname,
                                                var_choices = "WGRLOFL"
                                              ),
                                              selected = "Y", fixed = TRUE
                                            ),
                                            add_total = TRUE,
                                            drop_arm_levels = TRUE,
                                            pre_output = NULL,
                                            post_output = NULL,
                                            basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_abnormality_by_worst_grade")
  utils.nest::stop_if_not(
    assertthat::is.string(dataname),
    is.choices_selected(id_var),
    is.choices_selected(arm_var),
    is.choices_selected(paramcd),
    is.choices_selected(atoxgr_var),
    is.choices_selected(worst_high_flag_var),
    is.choices_selected(worst_low_flag_var),
    is.choices_selected(worst_flag_indicator),
    list(
      is.null(pre_output) || inherits(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || inherits(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE),
    atoxgr_var = cs_to_des_select(atoxgr_var, dataname = dataname),
    worst_high_flag_var = cs_to_des_select(worst_high_flag_var, dataname = dataname),
    worst_low_flag_var = cs_to_des_select(worst_low_flag_var, dataname = dataname)
  )

  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_abnormality_by_worst_grade,
    server = srv_t_abnormality_by_worst_grade,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_abnormality_by_worst_grade <- function(id, ...) { # nolint

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$paramcd,
    a$atoxgr_var,
    a$worst_high_flag_var,
    a$worst_low_flag_var,
    a$worst_flag_indicator
  )

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(teal.devel::table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(
        a[c(
          "arm_var", "id_var", "paramcd",
          "atoxgr_var", "worst_high_flag_var", "worst_low_flag_var", "worst_flag_indicator"
        )]
      ),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = FALSE),
      teal.devel::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Lab Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("atoxgr_var"),
        label = "Analysis toxicity grade",
        data_extract_spec = a$atoxgr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("worst_low_flag_var"),
        label = "Worst low flag variable",
        data_extract_spec = a$worst_low_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("worst_high_flag_var"),
        label = "Worst high flag variable",
        data_extract_spec = a$worst_high_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional table settings",
          teal.devel::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          optionalSelectInput(
            ns("worst_flag_indicator"),
            label = "Value Indicating Worst Grade",
            choices = a$worst_flag_indicator$choices,
            selected = a$worst_flag_indicator$selected,
            multiple = FALSE,
            fixed = a$worst_flag_indicator$fixed
          ),
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          )
        )
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_abnormality_by_worst_grade <- function(input, # nolint
                                             output,
                                             session,
                                             datasets,
                                             dataname,
                                             parentname,
                                             id_var,
                                             arm_var,
                                             paramcd,
                                             atoxgr_var,
                                             worst_low_flag_var,
                                             worst_high_flag_var,
                                             add_total,
                                             drop_arm_levels,
                                             label,
                                             basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  anl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(
      arm_var = arm_var, id_var = id_var, paramcd = paramcd,
      atoxgr_var = atoxgr_var, worst_high_flag_var = worst_high_flag_var,
      worst_low_flag_var = worst_low_flag_var
    ),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_paramcd_var <- as.vector(anl_m$columns_source$paramcd)
    input_atoxgr <- as.vector(anl_m$columns_source$atoxgr_var)
    input_worst_high_flag_var <- as.vector(anl_m$columns_source$worst_high_flag_var)
    input_worst_low_flag_var <- as.vector(anl_m$columns_source$worst_low_flag_var)

    validate(
      need(input_worst_high_flag_var, "Please select the Worst High Grade flag variable."),
      need(input_worst_low_flag_var, "Please select the Worst Low Grade flag variable."),
      need(
        !utils.nest::is_empty(anl_m$data()[[input_paramcd_var]]),
        "Please select at least one Laboratory parameter."
      ),
      need(input_atoxgr, "Please select Analysis Toxicity Grade variable."),
      need(input_id_var, "Please select a Subject Identifier."),
      need(input$worst_flag_indicator, "Please select the value indicating worst grade."),
      need(is.factor(anl_m$data()[[input_arm_var]]), "Treatment variable should be a factor."),
      need(is.factor(anl_m$data()[[input_paramcd_var]]), "Parameter variable should be a factor."),
      need(is.factor(anl_m$data()[[input_atoxgr]]), "Grade variable should be a factor."),
      need(
        all(as.character(unique(anl_m$data()[[input_atoxgr]])) %in% as.character(c(-4:4))),
        "All grade values should be within -4:4 range."
      )
    )

    # validate inputs
    teal.devel::validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c(
        "USUBJID", "STUDYID", input_paramcd_var,
        input_atoxgr, input_worst_high_flag_var,
        input_worst_low_flag_var
      ),
      arm_var = input_arm_var
    )
  })

  call_preparation <- reactive({
    validate_checks()
    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()
    anl_adsl <- adsl_merged()
    teal.devel::chunks_push_data_merge(anl_adsl)
    teal.devel::chunks_push_new_line()

    my_calls <- template_abnormality_by_worst_grade(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      id_var = as.vector(anl_m$columns_source$id_var),
      paramcd = as.vector(anl_m$columns_source$paramcd),
      atoxgr_var = as.vector(anl_m$columns_source$atoxgr_var),
      worst_high_flag_var = as.vector(anl_m$columns_source$worst_high_flag_var),
      worst_low_flag_var = as.vector(anl_m$columns_source$worst_low_flag_var),
      worst_flag_indicator = input$worst_flag_indicator,
      add_total = input$add_total,
      drop_arm_levels = input$drop_arm_levels,
      basic_table_args = basic_table_args
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
    teal.devel::chunks_get_var("result")
  })

  callModule(
    teal.devel::table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(
        arm_var, id_var, paramcd,
        atoxgr_var, worst_high_flag_var, worst_low_flag_var
      )
    ),
    modal_title = "R Code for Grade Laboratory Abnormalities",
    code_header = label
  )
}
