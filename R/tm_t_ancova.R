#' ANCOVA Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @name ancova
#'
NULL

#' @describeIn ancova create the expression corresponding to the analysis.
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(tern)
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#'
#' # Multiple endpoints at multiple timepoints
#' a <- teal.modules.clinical:::template_ancova(
#'   parentname = "adsl",
#'   dataname = "adqs",
#'   avisit = c("WEEK 1 DAY 8", "WEEK 2 DAY 15"),
#'   arm_var = "ARMCD",
#'   ref_arm = "ARM A",
#'   cov_var = c("BASE", "STRATA1"),
#'   aval_var = "CHG",
#'   paramcd = c("BFIALL", "FATIGI")
#' )
#'
#' styled_expr(a$data)
#' styled_expr(a$layout)
#' styled_expr(a$table)
#'
#' b <- mapply(expr = a, FUN = eval)
#' b$table
#'
#' # Single endpoint at single timepoint
#' a <- teal.modules.clinical:::template_ancova(
#'   parentname = "adsl",
#'   dataname = "adqs",
#'   avisit = "WEEK 1 DAY 8",
#'   arm_var = "ARMCD",
#'   ref_arm = "ARM A",
#'   cov_var = c("BASE", "STRATA1"),
#'   aval_var = "CHG",
#'   paramcd = "FKSI-FWB"
#' )
#'
#' styled_expr(a$data)
#' styled_expr(a$layout)
#' styled_expr(a$table)
#'
#' b <- mapply(expr = a, FUN = eval)
#' b$table
#'
template_ancova <- function(parentname, # nousage
                            dataname,
                            arm_var,
                            ref_arm,
                            cov_var,
                            aval_var,
                            avisit,
                            paramcd,
                            conf_level = 0.95
) {

  y <- list()

  # Data processing.
  y$data <- substitute(
    expr = anl <- df %>%
      filter(AVISIT %in% avisit & PARAMCD %in% paramcd) %>%
      droplevels(),
    env = list(
      df = as.name(dataname),
      avisit = avisit,
      paramcd = paramcd
    )
  )

  # Build layout.
  layout_list <- list()

  layout_list <- add_expr(layout_list, substitute(basic_table()))

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var, ref_group = ref_arm),
      env = list(
        arm_var = arm_var,
        ref_arm = ref_arm
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_rows_by("AVISIT", split_fun = drop_split_levels)
    )
  )

  if (length(unique(paramcd)) > 1) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by("PARAMCD") %>%
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = cov_var),
            conf_level = conf_level,
            var_labels = "Adjusted mean",
            show_labels = "hidden"
          ),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          cov_var = cov_var,
          conf_level = conf_level
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        summarize_ancova(
          vars = aval_var,
          variables = list(arm = arm_var, covariates = NULL),
          conf_level = conf_level,
          var_labels = "Unadjusted comparison",
          .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
        ) %>%
        summarize_ancova(
          vars = aval_var,
          variables = list(arm = arm_var, covariates = cov_var),
          conf_level = conf_level,
          var_labels = paste0(
            "Adjusted comparison (", paste(cov_var, collapse = " + "), ")"
          )
        ),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          cov_var = cov_var,
          conf_level = conf_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Build table.
  col_counts <- substitute(
    expr = table(parentname$arm_var),
    env = list(parentname = as.name(parentname), arm_var = arm_var)
  )
  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts),
    env = list(col_counts = col_counts)
  )

  y
}
