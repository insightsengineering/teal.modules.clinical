#' Adverse Events Table by Standardized MedDRA Query
#'
#' @inheritParams template_arguments
#' @param sort_criteria (`character`)\cr how to sort the final table. Default option `freq_desc` sorts
#'  by decreasing total number of patients with event. Alternative option `alpha` sorts events
#'  alphabetically.
#' @param smq_varlabel (`character`)\cr label of the new column `SMQ`
#' created by [tern::h_stack_by_baskets()].
#' @param baskets (`character`)\cr
#' variable names of the selected Standardized/Customized queries
#'
#' @seealso [tm_t_smq()]
#'
template_smq <- function(
  dataname,
  parentname,
  arm_var,
  llt = "AEDECOD",
  add_total = TRUE,
  sort_criteria = c("freq_desc", "alpha"),
  drop_arm_levels = TRUE,
  na_level = "<Missing>",
  smq_varlabel = "Standardized MedDRA Query",
  baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
  id_var = "USUBJID"
) {

  assert_that(
    is.string(parentname),
    is.string(dataname),
    is.character(arm_var) && length(arm_var) %in% c(1, 2),
    is.string(id_var),
    is.string(llt),
    is.flag(add_total),
    is.flag(drop_arm_levels),
    is.string(na_level),
    is.string(smq_varlabel),
    is.character(baskets)
  )

  sort_criteria <- match.arg(sort_criteria)

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- dataname,
      env = list(
        dataname = as.name(dataname)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var[[1]],
      drop_arm_levels = drop_arm_levels
    )
  )

  if (length(arm_var) == 2) {
    data_list <- add_expr(
      data_list,
      prepare_arm_levels(
        dataname = "anl",
        parentname = parentname,
        arm_var = arm_var[[2]],
        drop_arm_levels = drop_arm_levels
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- h_stack_by_baskets(
        df = dataname,
        baskets = baskets,
        smq_varlabel = smq_varlabel,
        keys = c("STUDYID", id_var, arm_var, llt)
      ),
      env = list(
        dataname = as.name("anl"),
        baskets = baskets,
        smq_varlabel = smq_varlabel,
        id_var = id_var,
        arm_var = arm_var,
        llt = llt
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- df_explicit_na(
        dataname,
        na_level = na_level),
      env = list(dataname = as.name("anl"),
                 na_level = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(
        parentname,
        na_level = na_level),
      env = list(
        parentname = as.name(parentname),
        na_level = na_level
        )
      )
  )

  y$data <- bracket_expr(data_list)

  # layout start
  y$layout_prep <- quote(split_fun <- drop_split_levels)

  # Start layout steps.
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = basic_table() %>%
          split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ),
        env = list(arm_var = arm_var[[1]])
      )
    } else {
      substitute(
        expr = basic_table() %>%
          split_cols_by(var = arm_var),
        env = list(arm_var = arm_var[[1]])
      )
    }
  )

  if (length(arm_var) == 2) {
    layout_list <- add_expr(
      layout_list,
      if (drop_arm_levels) {
        substitute(
          expr = split_cols_by(var = nested_col, split_fun = drop_split_levels),
          env = list(nested_col = arm_var[[2]])
        )
      } else {
        substitute(
          expr = split_cols_by(var = nested_col),
          env = list(nested_col = arm_var[[2]])
        )
      }
    )
  }

  layout_list <- add_expr(
    layout_list,
    quote(add_colcounts())
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_num_patients(
        var = id_var,
        .stats = c("unique"),
        .labels = c(
          unique = "Total number of patients with at least one adverse event"
        )
      ),
      env = list(
        id_var = id_var
        )
    )
  )

  split_label <- substitute(
    expr = var_labels(dataname)[["SMQ"]],
    env = list(
      dataname = as.name("anl")
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_rows_by(
        "SMQ",
        child_labels = "visible",
        nested = FALSE,
        indent_mod = -1L,
        split_fun = split_fun,
        label_pos = "topleft",
        split_label = split_label
      ),
      env = list(
        split_label = split_label
       )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_num_patients(
        var = id_var,
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = "Total number of patients with at least one adverse event",
          nonunique = "Total number of events"
        )
      ),
      env = list(
        id_var = id_var
        )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = count_occurrences(vars = llt),
      env = list(
        llt = llt
        )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = append_varlabels(dataname, llt, indent = 1L),
      env = list(
        dataname = as.name("anl"),
        llt = llt
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  if (sort_criteria == "freq_desc") {
    y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent) %>%
        sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
        sort_at_path(path = c("SMQ", "*", llt), scorefun = score_occurrences)
      result
    },
    env = list(
      parent = as.name(parentname),
      llt = llt
      )
    )} else {
      y$table <- substitute(
        expr = {
          result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent)
          result
        },
        env = list(
          parent = as.name(parentname)
        )
      )
    }

  y

}

#' Teal Module: SMQ Table
#'
#' @inheritParams module_arguments
#' @inheritParams template_smq
#' @param baskets ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#' object with all available choices and preselected options for Standardized/Customized queries
#' @param scopes ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#' object with all available choices for the scopes of Standardized queries.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adae <- synthetic_cdisc_data("latest")$adae
#'
#' names_baskets <- grep("^(SMQ|CQ).*NAM$", names(adae), value = TRUE)
#' names_scopes <- grep("^SMQ.*SC$", names(adae), value = TRUE)
#'
#' cs_baskets <- choices_selected(
#' choices = variable_choices(adae, subset = names_baskets),
#' selected = names_baskets
#' )
#'
#' cs_scopes <- choices_selected(
#' choices = variable_choices(adae, subset = names_scopes),
#' selected = names_scopes,
#' fixed = TRUE
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl,
#'     code = "ADSL <- synthetic_cdisc_data('latest')$adsl"
#'     ),
#'     cdisc_dataset("ADAE", adae,
#'       code = "ADAE <- synthetic_cdisc_data('latest')$adae"
#'       ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_smq(
#'       label = "Adverse events by SMQ Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "SEX")),
#'         selected = "ARM"
#'       ),
#'       add_total = FALSE,
#'       baskets = cs_baskets,
#'       scopes = cs_scopes,
#'       llt = choices_selected(
#'         choices = variable_choices(adae, subset = c("AEDECOD")),
#'         selected = "AEDECOD"
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'
tm_t_smq <- function(label,
                     dataname,
                     parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                     arm_var,
                     id_var = choices_selected(
                       variable_choices(dataname, subset = "USUBJID"), selected = "USUBJID", fixed = TRUE
                     ),
                     llt,
                     add_total = TRUE,
                     sort_criteria = c("freq_desc", "alpha"),
                     drop_arm_levels = TRUE,
                     na_level = "<Missing>",
                     smq_varlabel = "Standardized MedDRA Query",
                     baskets,
                     scopes,
                     pre_output = NULL,
                     post_output = NULL) {
  stop_if_not(
    is.string(dataname),
    is.choices_selected(arm_var),
    is.flag(add_total),
    is.flag(drop_arm_levels),
    is.choices_selected(id_var),
    is.choices_selected(llt),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  sort_criteria <- match.arg(sort_criteria)

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    baskets = cs_to_des_select(baskets, dataname = dataname, multiple = TRUE),
    scopes = cs_to_des_select(scopes, dataname = dataname, multiple = TRUE),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_smq,
    server = srv_t_smq,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        na_level = na_level,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_smq <- function(id, ...) {

  ns <- NS(id)
  a <- list(...) # module args


  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$id_var,
    a$baskets,
    a$scopes,
    a$llt
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c(
        "arm_var", "baskets", "llt", "id_var", "scopes"
      )]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("llt"),
        label = "Select the low level term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      data_extract_input(
        id = ns("baskets"),
        label = "Select the SMQXXNAM/CQXXNAM baskets",
        data_extract_spec = a$baskets,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional Variables Info",
          checkboxInput(ns(
            "drop_arm_levels"),
            "Drop arm levels not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          data_extract_input(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("scopes"),
            label = "Scope variables available",
            data_extract_spec = a$scopes,
            is_single_dataset = is_single_dataset_value
          ),
          selectInput(
            inputId = ns("sort_criteria"),
            label = "Sort Criteria",
            choices = c("Decreasing frequency" = "freq_desc",
                        "Alphabetically" = "alpha"
            ),
            selected = a$sort_criteria,
            multiple = FALSE
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_smq <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      parentname,
                      arm_var,
                      llt,
                      id_var,
                      baskets,
                      scopes,
                      na_level,
                      label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, id_var, baskets, scopes, llt),
    input_id = c("arm_var", "id_var", "baskets", "scopes", "llt"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  arm_var_user_input <- get_input_order("arm_var", arm_var$dataname)

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    anl_m <- anl_merged()
    anl_adsl <- adsl_merged()

    input_arm_var <- arm_var_user_input()
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_baskets <- as.vector(anl_m$columns_source$baskets)
    input_scopes <- as.vector(anl_m$columns_source$scopes)
    input_llt <- as.vector(anl_m$columns_source$llt)

    validate(
      need(input_id_var, "Please select a subject identifier."),
      need(input_baskets, "Please select the SMQ/CQ baskets."),
      need(input_scopes, "Please select the scope variables."),
      need(input_llt, "Please select the low level term.")
    )
    #validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c(
        "USUBJID", "STUDYID", input_id_var, input_baskets,
        input_scopes, input_llt
        ),
      arm_var = input_arm_var[[1]]
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

    my_calls <- template_smq(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = arm_var_user_input(),
      llt = as.vector(anl_m$columns_source$llt),
      add_total = input$add_total,
      sort_criteria = input$sort_criteria,
      drop_arm_levels = input$drop_arm_levels,
      baskets = as.vector(anl_m$columns_source$baskets),
      na_level = na_level,
      id_var = as.vector(anl_m$columns_source$id_var),

    )
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
      list(arm_var, id_var, baskets, scopes, llt)
    ),
    modal_title = "R Code for SMQ tables",
    code_header = label
  )
}
