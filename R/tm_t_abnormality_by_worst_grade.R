#' Template: Laboratory test results with highest grade post-baseline
#' @inheritParams template_arguments
#' @param grade_dir_var (`character`)\cr the variable name indicating
#' Analysis Reference Range Indicator.
#' @param atoxgr_var (`character`)\cr the variable name indicating
#' Analysis Toxicity Grade.
#' @param worst_high_flag_var (`character`)\cr the variable name indicating
#' Worst High Grade flag
#' @param worst_low_flag_var (`character`)\cr the variable name indicating
#' Worst Low Grade flag
#' @param worst_flag_indicator (`character`)\cr value indicating worst grade.
#'
#' @seealso [tm_t_abnormality_by_worst_grade()]
template_abnormality_by_worst_grade <- function(parentname, #nolint
                                                dataname,
                                                arm_var,
                                                id_var = "USUBJID",
                                                paramcd = "PARAMCD",
                                                grade_dir_var = "ANRIND",
                                                atoxgr_var = "ATOXGR",
                                                worst_high_flag_var = "WGRHIFL",
                                                worst_low_flag_var = "WGRLOFL",
                                                worst_flag_indicator = "Y",
                                                add_total = FALSE,
                                                drop_arm_levels = TRUE) {


  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl_labels <- var_labels(df),
      env = list(
        df = as.name(dataname)
      )
    )
  )

  data_list <- add_expr(data_list,
                        substitute(
                          expr = anl <- df %>%
                            mutate(
                              WGRLOFL = case_when(worst_low_flag_var == worst_flag_indicator ~ TRUE, TRUE ~ FALSE),
                              WGRHIFL = case_when(worst_high_flag_var == worst_flag_indicator ~ TRUE, TRUE ~ FALSE),
                              #Changed the following prepo step methodology as not
                              #all cases have grade = 4 (realized with nsdl real data)
                              GRADE_DIR = factor(
                                case_when(
                                  as.numeric(as.character(atoxgr_var)) < 0 ~ "LOW",
                                  atoxgr_var == "0" ~ "ZERO",
                                  as.numeric(as.character(atoxgr_var)) > 0 ~ "HIGH"
                                ),
                                levels = c("LOW", "ZERO", "HIGH")
                              ),
                              #Changed the following prepo step methodology as not
                              #all cases have grade = 4 (realized with nsdl real data)
                              GRADE_ANL = factor(abs(as.numeric(
                                as.character(atoxgr_var)
                              )))
                            ) %>%
                            filter(WGRLOFL == TRUE | WGRHIFL == TRUE) %>%
                            droplevels(),
                          env  = list(
                            df = as.name(dataname),
                            worst_low_flag_var = as.name(worst_low_flag_var),
                            worst_high_flag_var = as.name(worst_high_flag_var),
                            worst_flag_indicator = worst_flag_indicator,
                            atoxgr_var = as.name(atoxgr_var)
                          )
                        ))

  data_list <- add_expr(data_list,
                        quote(expr = var_labels(anl) <-
                                c(anl_labels, "GRADE_DIR", "GRADE_ANL")))

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
        arrange(paramcd, desc(GRADE_DIR), GRADE_ANL),
      env = list(
        paramcd = paramcd
      )
    )
  )

  y$layout_prep <- bracket_expr(prep_list)

# layout start

  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = basic_table() %>%
          split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ) %>%
          add_colcounts(),
        env = list(arm_var = arm_var)
      )
    } else {
      substitute(
        expr = basic_table() %>%
          split_cols_by(var = arm_var) %>%
          add_colcounts(),
        env = list(arm_var = arm_var)
      )
    }
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_rows_by(
        paramcd,
        label_pos = "topleft",
        split_label = obj_label(anl[[paramcd]])
        ) %>%
        summarize_num_patients(
          var = id_var,
          required = "GRADE_ANL",
          .stats = "unique_count"
        ) %>%
        split_rows_by(
          "GRADE_DIR",
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map),
          split_label = "Direction of Abnormality"
          ) %>%
        count_abnormal_by_worst_grade(
          var = "GRADE_ANL",
          variables = list(id = id_var, param  = paramcd, grade_dir = "GRADE_DIR")
        ) %>%
        append_topleft("    Highest NCI CTCAE Grade"),
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
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent)
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
#' @param grade_dir_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#' object with all available choices and preselected option
#' for variable names that can be used as Analysis Reference Range Indicator.
#' @param atoxgr_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#' object with all available choices and preselected option
#' for variable names that can be used as Analysis Toxicity Grade.
#' @param worst_high_flag_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr object with all available
#' choices and preselected option for variable names that can be used as Worst High Grade flag.
#' @param worst_low_flag_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr object with all available
#' choices and preselected option for variable names that can be used as Worst Low Grade flag.
#' @param worst_flag_indicator ([teal::choices_selected()] or [teal::data_extract_spec])\cr value indicating
#' worst grade.
#' @seealso [template_abnormality_by_worst_grade()]
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adlb <- synthetic_cdisc_data("latest")$adlb %>%
#' filter(!AVISIT %in% c("SCREENING", "BASELINE"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADLB", adlb,
#'                   code = 'ADLB <- synthetic_cdisc_data("latest")$adlb %>%
#' filter(!AVISIT %in% c("SCREENING", "BASELINE"))'),
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
#'    )
#'  ),
#'  filter = (
#'  list(
#'  ADSL = list(SAFFL = "Y"),
#'  ADLB = list(ONTRTFL = "Y")
#'  )
#'  )
#' )
#'
#'  \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality_by_worst_grade <- function(label, #nolint
                                            dataname,
                                            parentname = ifelse(
                                              is(arm_var, "data_extract_spec"),
                                              datanames_input(arm_var),
                                              "ADSL"
                                            ),
                                            arm_var,
                                            id_var = choices_selected(
                                              variable_choices(
                                                dataname, subset = "USUBJID"
                                              ),
                                              selected = "USUBJID", fixed = TRUE
                                            ),
                                            paramcd,
                                            grade_dir_var = choices_selected(
                                              variable_choices(
                                                dataname,
                                                subset = "ANRIND"
                                              ),
                                              selected = "ANRIND", fixed = TRUE
                                            ),
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
                                                subset = "WGRLOFL"
                                              ),
                                              selected = "Y", fixed = TRUE
                                            ),
                                            add_total = TRUE,
                                            exclude_base_abn = FALSE,
                                            drop_arm_levels = TRUE,
                                            pre_output = NULL,
                                            post_output = NULL
                                            ) {

  stop_if_not(
    is.string(dataname),
    is.choices_selected(id_var),
    is.choices_selected(arm_var),
    is.choices_selected(paramcd),
    is.choices_selected(grade_dir_var),
    is.choices_selected(atoxgr_var),
    is.choices_selected(worst_high_flag_var),
    is.choices_selected(worst_low_flag_var),
    is.choices_selected(worst_flag_indicator),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE),
    grade_dir_var = cs_to_des_select(grade_dir_var, dataname = dataname),
    atoxgr_var = cs_to_des_select(atoxgr_var, dataname = dataname),
    worst_high_flag_var = cs_to_des_select(worst_high_flag_var, dataname = dataname),
    worst_low_flag_var = cs_to_des_select(worst_low_flag_var, dataname = dataname)
  )

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
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )

}

#' @noRd
ui_t_abnormality_by_worst_grade <- function(id, ...) { #nolint

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$id_var,
    a$paramcd,
    a$grade_dir_var,
    a$atoxgr_var,
    a$worst_high_flag_var,
    a$worst_low_flag_var,
    a$worst_flag_indicator
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(
        a[c(
          "arm_var", "id_var", "paramcd", "grade_dir_var",
          "atoxgr_var", "worst_high_flag_var", "worst_low_flag_var", "worst_flag_indicator"
        )]
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = FALSE),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Lab Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("worst_low_flag_var"),
        label = "Worst low flag variable",
        data_extract_spec = a$worst_low_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("worst_high_flag_var"),
        label = "Worst high flag variable",
        data_extract_spec = a$worst_high_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("grade_dir_var"),
        label = "Analysis Reference Range Indicator",
        data_extract_spec = a$grade_dir_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("atoxgr_var"),
        label = "Analysis toxicity grade",
        data_extract_spec = a$atoxgr_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional table settings",
          data_extract_input(
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
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_abnormality_by_worst_grade <- function(input, #nolint
                                             output,
                                             session,
                                             datasets,
                                             dataname,
                                             parentname,
                                             id_var,
                                             arm_var,
                                             paramcd,
                                             grade_dir_var,
                                             atoxgr_var,
                                             worst_low_flag_var,
                                             worst_high_flag_var,
                                             add_total,
                                             drop_arm_levels,
                                             label) {

  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(
      arm_var, id_var, paramcd, grade_dir_var, atoxgr_var, worst_high_flag_var, worst_low_flag_var
    ),
    input_id = c(
      "arm_var", "id_var", "paramcd", "grade_dir_var", "atoxgr_var", "worst_high_flag_var", "worst_low_flag_var"
    ),
    merge_function = "dplyr::inner_join")

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_paramcd_var <- as.vector(anl_m$columns_source$paramcd)
    input_anrind <- as.vector(anl_m$columns_source$grade_dir_var)
    input_atoxgr <- as.vector(anl_m$columns_source$atoxgr_var)
    input_worst_high_flag_var <- as.vector(anl_m$columns_source$worst_high_flag_var)
    input_worst_low_flag_var <- as.vector(anl_m$columns_source$worst_low_flag_var)

    validate(
      need(input_worst_high_flag_var, "Please select the Worst High Grade flag variable."),
      need(input_worst_low_flag_var, "Please select the Worst Low Grade flag variable."),
      need(!is_empty(anl_m$data()[[input_paramcd_var]]), "Please select at least one Laboratory parameter."),
      need(input_anrind, "Please select Analysis Reference Range Indicator variable."),
      need(input_atoxgr, "Please select Analysis Toxicity Grade variable."),
      need(input_id_var, "Please select a Subject Identifier."),
      need(input$worst_flag_indicator, "Please select the value indicating worst grade.")
    )

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c(
        "USUBJID", "STUDYID", input_paramcd_var,
        input_anrind, input_anrind, input_worst_high_flag_var,
        input_worst_low_flag_var
      ),
      arm_var = input_arm_var
    )

  })

  call_preparation <- reactive({
    validate_checks()
    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()
    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    my_calls <- template_abnormality_by_worst_grade(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      id_var = as.vector(anl_m$columns_source$id_var),
      paramcd = as.vector(anl_m$columns_source$paramcd),
      grade_dir_var = as.vector(anl_m$columns_source$grade_dir_var),
      atoxgr_var = as.vector(anl_m$columns_source$atoxgr_var),
      worst_high_flag_var = as.vector(anl_m$columns_source$worst_high_flag_var),
      worst_low_flag_var =  as.vector(anl_m$columns_source$worst_low_flag_var),
      worst_flag_indicator = input$worst_flag_indicator,
      add_total = input$add_total,
      drop_arm_levels = input$drop_arm_levels)
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(
        arm_var, id_var, paramcd, grade_dir_var,
        atoxgr_var, worst_high_flag_var, worst_low_flag_var
        )
    ),
    modal_title = "R Code for Grade Laboratory Abnormalities",
    code_header = label
  )
}
