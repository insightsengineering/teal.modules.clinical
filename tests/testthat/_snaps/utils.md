# h_concat_expr returns a string for long expression

    Code
      res
    Output
      [1] "rtables::basic_table() %>% rtables::split_cols_by(var = \"ARMCD\") %>%      tern::test_proportion_diff(vars = \"rsp\", method = \"cmh\",          variables = list(strata = \"strat\")) %>% rtables::build_table(df = dta)"

# add_expr adds expressions to expression list

    Code
      res
    Output
      [[1]]
      rtables::basic_table()
      
      [[2]]
      rtables::split_cols_by(var = arm)
      
      [[3]]
      tern::test_proportion_diff(vars = "rsp", method = "cmh", variables = list(strata = "strat"))
      
      [[4]]
      rtables::build_table(df = dta)
      

# add_expr manages expression list which can be used by pipe_expr

    Code
      res
    Output
      rtables::basic_table() %>% rtables::split_cols_by(var = arm) %>% 
          test_proportion_diff(vars = "rsp", method = "cmh", variables = list(strata = "strat")) %>% 
          rtables::build_table(df = dta)

# bracket_expr concatenates expressions into a single expression

    Code
      res
    Output
      {
          anl <- subset(adrs, PARAMCD == "INVET")
          anl$rsp_lab <- tern::d_onco_rsp_label(anl$AVALC)
          anl$is_rsp <- anl$rsp_lab %in% c("Complete Response (CR)", 
              "Partial Response (PR)")
      }

# bracket_expr returns a single evaluable expression

    Code
      res
    Output
                                
                                 FALSE TRUE
        Complete Response (CR)       0   60
        Partial Response (PR)        0   45
        Stable Disease (SD)         50    0
        Progressive Disease (PD)    39    0
        Not Evaluable (NE)           6    0

# prepare_arm with standard inputs

    Code
      res
    Output
      adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>% 
          dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>% 
          dplyr::mutate(ARMCD = droplevels(ARMCD))

# prepare_arm combine ref arms

    Code
      res
    Output
      adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>% 
          dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", 
              "ARM B"), new_level = "ARM A/ARM B")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
          ref = "ARM A/ARM B")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD))

# prepare_arm combine ref arms and use new level

    Code
      res
    Output
      adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>% 
          dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", 
              "ARM B"), new_level = "Control")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
          ref = "Control")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD))

# prepare_arm_levels with standard inputs

    Code
      res
    Output
      {
          adae <- adae %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(adae[["ARMCD"]])
          adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      }

# prepare_arm_levels can use parentname arm levels

    Code
      res
    Output
      {
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(adsl[["ARMCD"]])
          adae <- adae %>% dplyr::mutate(ARMCD = factor(ARMCD, levels = arm_levels))
      }

# color_lab_values main

    Code
      res
    Output
                                                                                           3 HIGH 
         "<span style='color:red!important'>3<i class='glyphicon glyphicon-arrow-up'></i></span>" 
                                                                                         2 NORMAL 
                                "<span style='color:grey!important'>2<i class='NULL'></i></span>" 
                                                                                           5 HIGH 
         "<span style='color:red!important'>5<i class='glyphicon glyphicon-arrow-up'></i></span>" 
                                                                                                4 
                                                                                              "4" 
                                                                                            0 LOW 
      "<span style='color:blue!important'>0<i class='glyphicon glyphicon-arrow-down'></i></span>" 

