# template_medical_history - non-default parameters

    Code
      res
    Output
      {
          labels <- formatters::var_labels(anl, fill = FALSE)[c("mhbodsys", 
              "mhterm", "mhdistat")]
          mhbodsys_label <- labels["mhbodsys"]
          result_raw <- anl %>% dplyr::select(mhbodsys, mhterm, mhdistat) %>% 
              dplyr::arrange(mhbodsys) %>% dplyr::mutate_if(is.character, 
              as.factor) %>% dplyr::mutate_if(is.factor, function(x) explicit_na(x, 
              "UNKNOWN")) %>% dplyr::distinct() %>% `colnames<-`(labels)
          result <- rtables::basic_table() %>% rtables::split_cols_by_multivar(colnames(result_raw)[2:3]) %>% 
              rtables::split_rows_by(colnames(result_raw)[1], split_fun = rtables::drop_split_levels) %>% 
              rtables::split_rows_by(colnames(result_raw)[2], split_fun = rtables::drop_split_levels, 
                  child_labels = "hidden") %>% rtables::analyze_colvars(function(x) x[seq_along(x)]) %>% 
              rtables::build_table(result_raw)
          result
      }

