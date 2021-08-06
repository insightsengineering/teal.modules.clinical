#' Title
#'
#' @param parentname
#' @param dataname
#' @param arm_var
#' @param basket
#' @param llt
#' @param add_total
#' @param drop_arm_levels
#'
#' @return
#' @export
#'
template_smq <- function(
  parentname,
  dataname,
  arm_var,
  id_var = "USUBJID",
  col_by_var,
  llt = "AEDECOD",
  add_total = TRUE,
  drop_arm_levels = TRUE,
  na_level = "<Missing>",
  smq_varlabel = "Standardized MedDRA Query",
  baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
  keys = c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM"),
  sort_by_descending = TRUE
) {

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
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      df_stack <- h_stack_by_baskets(
        df = dataname,
        baskets = baskets
      ),
      env = list(
        dataname = as.name("anl"),
        baskets = baskets
      )
    )
  )

  #merging with ADSL for obtaining ARM and col_by_var (if not NULL)
  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- inner_join(x = dataname, y = df_stack, by = keys),
      env = list(
        dataname = as.name("anl"),
        keys = keys
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
        env = list(arm_var = arm_var)
      )
    } else {
      substitute(
        expr = basic_table() %>%
          split_cols_by(var = arm_var),
        env = list(arm_var = arm_var)
      )
    }
  )

  if (is.null(col_by_var)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = add_colcounts()
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = split_cols_by(var = col_by_var) %>%
          add_colcounts(),
        env = list(col_by_var = col_by_var)
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_num_patients(
        var = id_var,
        .stats = c("unique"),
        .labels = c(
          unique = "Total number of patients with at least one adverse event"
        )
      ) ,
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
      ) ,
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
        dataname = anl,
        llt = llt
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  if (sort_by_descending) {
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

#' Title
#'
#' @param label
#' @param dataname
#' @param parentname
#' @param arm_var
#' @param id_var
#' @param col_by_var
#' @param llt
#' @param add_total
#' @param drop_arm_levels
#' @param na_level
#' @param smq_varlabel
#' @param baskets
#' @param keys
#' @param sort_by_descending
#' @param pre_output
#' @param post_output
#'
#' @return
#' @export
#'
#' @examples
#' library(scda)
#'
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
#' selected = names_scopes
#' )
#'
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl,
#'     code = 'ADSL <- radsl(cached = TRUE)'),
#'     cdisc_dataset("ADLB", adae,
#'       code = 'ADAE <- synthetic_cdisc_data("latest")$adae'
#'       )
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_abnormality(
#'       label = "Adverse events by SMQ Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       add_total = FALSE,
#'       col_by_var = choices_selected(
#'         choices = variable_choices(adae, subset = c("SEX", "STRATA1")),
#'         selected = NULL
#'       ),
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
                     col_by_var,
                     llt,
                     add_total = TRUE,
                     drop_arm_levels = TRUE,
                     na_level = "<Missing>",
                     smq_varlabel = "Standardized MedDRA Query",
                     baskets,
                     scopes,
                     keys = c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM"),
                     sort_by_descending = TRUE,
                     pre_output = NULL,
                     post_output = NULL) {
  stop_if_not(
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
    col_by_var = cs_to_des_select(col_by_var, dataname = parentname),
    baskets = cs_to_des_select(baskets, dataname = dataname),
    scopes = cs_to_des_select(scopes, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname),
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
ui_t_exposure <- function(id, ...) {

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$col_by_var,
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
        "arm_var", "col_by_var", "id_var", "baskets", "llt"
      )]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      data_extract_input(
        id = ns("col_by_var"),
        label = "Select additional column by variable, if desired",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("baskets"),
        label = "Select the SMQXXNAM/CQXXNAM baskets",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value,
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("scopes"),
        label = "Scope variables available",
        data_extract_spec = a$scopes,
        is_single_dataset = is_single_dataset_value,
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("llt"),
        label = "Select the low level term",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("sort_by_descending"), "Add All Patients column", value = a$sort_by_descending),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_exposure <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           parentname,
                           arm_var,
                           id_var,
                           col_by_var,
                           baskets,
                           scopes,
                           llt,
                           na_level,
                           label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, id_var, col_by_var, baskets, scopes, llt),
    input_id = c("id_var", "col_by_var", "baskets", "scopes", "llt"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(col_by_var, arm_var),
    input_id = c("col_by_var", "arm_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    anl_m <- anl_merged()
    anl_adsl <- adsl_merged()

    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_col_by_var <- as.vector(anl_adsl$columns_source$col_by_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_baskets <- as.vector(anl_m$columns_source$baskets)
    input_scopes <- as.vector(anl_m$columns_source$scopes)
    input_llt <- as.vector(anl_m$columns_source$llt)
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
      arm_var = as.vector(anl_m$columns_source$arm_var),
      id_var = as.vector(anl_m$columns_source$id_var),
      col_by_var = as.vector(anl_adsl$columns_source$col_by_var),
      baskets = as.vector(anl_m$columns_source$baskets),
      scopes = as.vector(anl_m$columns_source$scopes),
      llt = as.vector(anl_m$columns_source$llt),
      sort_by_descending = input$sort_by_descending,
      add_total = input$add_total,
      drop_arm_levels = input$drop_arm_levels,
      na_level = na_level
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
      list(arm_var, id_var, col_by_var, baskets, scopes, llt)
    ),
    modal_title = "R Code for SMQ tables",
    code_header = label
  )
}


