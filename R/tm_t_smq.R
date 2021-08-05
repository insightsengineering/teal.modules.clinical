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
  keys = c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM")
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

  # data_list <- add_expr(
  #   data_list,
  #   substitute(
  #     anl <- df_explicit_na(dataname, na_level = na_level),
  #     env = list(dataname = as.name(dataname),
  #                na_level = na_level
  #     )
  #   )
  # )
  #
  # data_list <- add_expr(
  #   data_list,
  #   substitute(
  #     parentname <- df_explicit_na(parentname, na_level = na_level),
  #     env = list(
  #       parentname = as.name(parentname),
  #       na_level = na_level
  #       )
  #     )
  # )

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

  if (is.null(col_by_var))
  {
    layout_list <- add_expr(
      layout_list,
      expr = quote(add_colcounts())
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = basic_table() %>%
          split_cols_by(var = col_by_var) %>%
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
        df = dataname,
        .stats = c("unique"),
        .labels = c(unique = "Total number of patients with at least one adverse event")
      ) ,
      env = list(
        id_var = id_var,
        dataname = "anl")
    )
  )

  y


  split_fun <- drop_split_levels

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae_smq1$SMQ)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD") %>%
    append_varlabels(adae_smq1, "AEDECOD", indent = 1L)

  result <- build_table(
    lyt = lyt,
    df = adae_smq1,
    alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  result


}
