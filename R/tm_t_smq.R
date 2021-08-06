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
#' @examples
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
                     keys = c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM"),
                     sort_by_descending = TRUE,
                     pre_output = NULL,
                     post_output = NULL) {
  # stop_if_not(
  #   is.string(dataname),
  #   is.choices_selected(arm_var),
  #   is.flag(add_total),
  #   is.choices_selected(by_vars),
  #   is.choices_selected(grade),
  #   is_character_vector(abnormal),
  #   is.choices_selected(id_var),
  #   is.choices_selected(baseline_var),
  #   is.choices_selected(treatment_flag),
  #   is.choices_selected(treatment_flag_var),
  #   is_logical_single(exclude_base_abn),
  #   is.flag(drop_arm_levels),
  #   list(
  #     is.null(pre_output) || is(pre_output, "shiny.tag"),
  #     "pre_output should be either null or shiny.tag type of object"
  #   ),
  #   list(
  #     is.null(post_output) || is(post_output, "shiny.tag"),
  #     "post_output should be either null or shiny.tag type of object"
  #   )
  # )

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    col_by_var = cs_to_des_select(col_by_var, dataname = parentname),
    baskets = cs_to_des_select(baskets, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname),
  )

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_abnormality,
    server = srv_t_abnormality,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        abnormal = abnormal,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

