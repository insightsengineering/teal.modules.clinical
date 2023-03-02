# template_shift_by_grade generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          anl <- adlb %>% dplyr::filter(WGRLOVFL == "Y")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- df_explicit_na(anl, na_level = "<Missing>")
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          by_visit <- TRUE
          anl <- dplyr::mutate(anl, ATOXGR_GP = factor(dplyr::case_when(ATOXGR %in% 
              c(0, 1, 2, 3, 4) ~ "Not Low", ATOXGR == -1 ~ "1", ATOXGR == 
              -2 ~ "2", ATOXGR == -3 ~ "3", ATOXGR == -4 ~ "4", ATOXGR == 
              "<Missing>" ~ "Missing")), BTOXGR_GP = factor(dplyr::case_when(BTOXGR %in% 
              c(0, 1, 2, 3, 4) ~ "Not Low", BTOXGR == -1 ~ "1", BTOXGR == 
              -2 ~ "2", BTOXGR == -3 ~ "3", BTOXGR == -4 ~ "4", BTOXGR == 
              "<Missing>" ~ "Missing")))
          anl <- dplyr::mutate(anl, ATOXGR_GP = factor(ATOXGR_GP, levels = c(dplyr::if_else("WGRLOVFL" %in% 
              c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"), "1", 
              "2", "3", "4", "Missing")), BTOXGR_GP = factor(BTOXGR_GP, 
              levels = c(dplyr::if_else("WGRLOVFL" %in% c("WGRLOVFL", 
                  "WGRLOFL"), "Not Low", "Not High"), "1", "2", "3", 
                  "4", "Missing")))
          column_labels <- list(PARAMCD = formatters::var_labels(anl, 
              fill = FALSE)[["PARAMCD"]], AVISIT = formatters::var_labels(anl, 
              fill = FALSE)[["AVISIT"]], ATOXGR_GP = dplyr::if_else(TRUE, 
              "Grade at Visit", "Post-baseline Grade"), BTOXGR_GP = "Baseline Grade")
          formatters::var_labels(anl)[names(column_labels)] <- as.character(column_labels)
          anl
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Grade Summary Table", subtitles = "Worst Flag Variable: WGRLOVFL") %>% 
          rtables::split_cols_by(var = "ARM") %>% rtables::add_colcounts() %>% 
          rtables::split_rows_by(var = "PARAMCD", split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(anl, 
                  fill = FALSE)[["PARAMCD"]]) %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(anl, 
              fill = FALSE)[["AVISIT"]]) %>% rtables::split_rows_by(var = "ATOXGR_GP", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(anl, 
              fill = FALSE)[["ATOXGR_GP"]]) %>% summarize_num_patients(var = "USUBJID", 
          .stats = c("unique_count")) %>% count_occurrences(vars = "BTOXGR_GP", 
          denom = "n", drop = TRUE, .indent_mods = 4L) %>% append_varlabels(anl, 
          "BTOXGR_GP", indent = 3L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>% 
              rtables::prune_table()
          result
      }
      

# template_shift_by_grade generates correct expressions with custom arguments

    Code
      res
    Output
      $data
      {
          anl <- adlb %>% dplyr::filter(WGRLOVFL == "YY")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- df_explicit_na(anl, na_level = "<MYMissing>")
          adsl <- df_explicit_na(adsl, na_level = "<MYMissing>")
          by_visit <- TRUE
          anl <- dplyr::mutate(anl, ATOXGR_GP = factor(dplyr::case_when(MYATOXGR %in% 
              c(0, 1, 2, 3, 4) ~ "Not Low", MYATOXGR == -1 ~ "1", MYATOXGR == 
              -2 ~ "2", MYATOXGR == -3 ~ "3", MYATOXGR == -4 ~ "4", 
              MYATOXGR == "<MYMissing>" ~ "Missing")), BTOXGR_GP = factor(dplyr::case_when(MYBTOXGR %in% 
              c(0, 1, 2, 3, 4) ~ "Not Low", MYBTOXGR == -1 ~ "1", MYBTOXGR == 
              -2 ~ "2", MYBTOXGR == -3 ~ "3", MYBTOXGR == -4 ~ "4", 
              MYBTOXGR == "<MYMissing>" ~ "Missing")))
          anl <- dplyr::mutate(anl, ATOXGR_GP = factor(ATOXGR_GP, levels = c(dplyr::if_else("WGRLOVFL" %in% 
              c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"), "1", 
              "2", "3", "4", "Missing")), BTOXGR_GP = factor(BTOXGR_GP, 
              levels = c(dplyr::if_else("WGRLOVFL" %in% c("WGRLOVFL", 
                  "WGRLOFL"), "Not Low", "Not High"), "1", "2", "3", 
                  "4", "Missing")))
          column_labels <- list(PARAMCD = formatters::var_labels(anl, 
              fill = FALSE)[["PARAMCD"]], AVISIT = formatters::var_labels(anl, 
              fill = FALSE)[["AVISIT"]], ATOXGR_GP = dplyr::if_else(TRUE, 
              "Grade at Visit", "Post-baseline Grade"), BTOXGR_GP = "Baseline Grade")
          formatters::var_labels(anl)[names(column_labels)] <- as.character(column_labels)
          anl
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Grade Summary Table", subtitles = "Worst Flag Variable: WGRLOVFL") %>% 
          rtables::split_cols_by(var = "ARM") %>% rtables::add_colcounts() %>% 
          rtables::split_rows_by(var = "PARAMCD", split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(anl, 
                  fill = FALSE)[["PARAMCD"]]) %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(anl, 
              fill = FALSE)[["AVISIT"]]) %>% rtables::split_rows_by(var = "ATOXGR_GP", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(anl, 
              fill = FALSE)[["ATOXGR_GP"]]) %>% summarize_num_patients(var = "MYUSUBJID", 
          .stats = c("unique_count")) %>% count_occurrences(vars = "BTOXGR_GP", 
          denom = "n", drop = TRUE, .indent_mods = 4L) %>% append_varlabels(anl, 
          "BTOXGR_GP", indent = 3L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>% 
              rtables::prune_table()
          result
      }
      

