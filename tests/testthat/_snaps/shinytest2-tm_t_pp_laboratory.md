# e2e - tm_t_pp_laboratory: Selecting patient_id changes the table and does not throw validation errors.

    Code
      testthat::expect_identical(table_before, app_driver$
        get_active_module_table_output("lab_values_table", which = 2))
    Condition
      Error:
      ! Expected `table_before` to be identical to `app_driver$get_active_module_table_output(...)`.
      Differences:
      actual vs expected
                            1       2       3       4       5       6       7
      - actual[1, ]   11.7355  2.4296 14.2567 35.2907  6.5769 11.8117 18.7490
      + expected[1, ] 25.3832 15.9318 23.1840 17.9912 27.2928 34.0837 23.6811
      - actual[2, ]    9.9637  8.1957  7.8251 10.2462  9.3719  8.4812  9.8576
      + expected[2, ]  8.9787  9.4720  9.3977  8.4470  8.6884  7.8815  8.5332
      - actual[3, ]    2.8477  3.0080  2.9235  2.6677  2.8257  3.0456  2.7825
      + expected[3, ]  2.9600  2.7764  2.9723  2.9331  2.9103  2.9610  2.8718
      
        `actual[[5]]`: 11.74 9.96 2.85
      `expected[[5]]`: 25.38 8.98 2.96
      
        `actual[[6]]`:  2.43 8.20 3.01
      `expected[[6]]`: 15.93 9.47 2.78
      
        `actual[[7]]`: 14.257 7.825 2.924
      `expected[[7]]`: 23.184 9.398 2.972
      
        `actual[[8]]`: 35.29 10.25 2.67
      `expected[[8]]`: 17.99  8.45 2.93
      
        `actual[[9]]`:  6.577 9.372 2.826
      `expected[[9]]`: 27.293 8.688 2.910
      
        `actual[[10]]`: 11.812 8.481 3.046
      `expected[[10]]`: 34.084 7.881 2.961
      
        `actual[[11]]`: 18.749 9.858 2.783
      `expected[[11]]`: 23.681 8.533 2.872

# e2e - tm_t_pp_laboratory: Selecting param changes the table  and does not throw validation errors.

    Code
      testthat::expect_identical(table_before, app_driver$
        get_active_module_table_output("lab_values_table", which = 2))
    Condition
      Error:
      ! Expected `table_before` to be identical to `app_driver$get_active_module_table_output(...)`.
      Differences:
      `names(actual)[1:6]`:   "" "PARAMCD" "PARAM" "AVALU" "1" "2"
      `names(expected)[1:6]`: "" "PARAMCD" "SEX"   "AVALU" "1" "2"
      
          actual[[3]]            | expected[[3]]    
      [1] "Alanine Aminotran..." - "M"           [1]
      [2] "C-Reactive Protei..." - "M"           [2]
      [3] "Immunoglobulin A ..." - "M"           [3]

# e2e - tm_t_pp_laboratory: Selecting timepoints changes the table  and does not throw validation errors.

    Code
      testthat::expect_identical(table_before, app_driver$
        get_active_module_table_output("lab_values_table", which = 2))
    Condition
      Error:
      ! Expected `table_before` to be identical to `app_driver$get_active_module_table_output(...)`.
      Differences:
      actual vs expected
                        
        actual[1, ]    1
      - actual[2, ]    2
      + expected[2, ]  8
      - actual[3, ]    3
      + expected[3, ] 15
      
        `actual[[1]]`: 1 2  3
      `expected[[1]]`: 1 8 15

# e2e - tm_t_pp_laboratory: Selecting avalu changes the table  and does not throw validation errors.

    Code
      testthat::expect_identical(table_before, app_driver$
        get_active_module_table_output("lab_values_table", which = 2))
    Condition
      Error:
      ! Expected `table_before` to be identical to `app_driver$get_active_module_table_output(...)`.
      Differences:
      `names(actual)[1:7]`:   "" "PARAMCD" "PARAM" "AVALU" "1" "2" "3"
      `names(expected)[1:7]`: "" "PARAMCD" "PARAM" "SEX"   "1" "2" "3"
      
      `actual[[4]]`:   "U/L" "mg/L" "g/L"
      `expected[[4]]`: "M"   "M"    "M"  

# e2e - tm_t_pp_laboratory: Selecting aval_var changes the table  and does not throw validation errors.

    Code
      testthat::expect_identical(table_before, app_driver$
        get_active_module_table_output("lab_values_table", which = 2))
    Condition
      Error:
      ! Expected `table_before` to be identical to `app_driver$get_active_module_table_output(...)`.
      Differences:
      actual vs expected
                            1       2       3       4       5       6       7
      - actual[1, ]   11.7355  2.4296 14.2567 35.2907  6.5769 11.8117 18.7490
      + expected[1, ] 32.0000 32.0000 32.0000 32.0000 32.0000 32.0000 32.0000
      - actual[2, ]    9.9637  8.1957  7.8251 10.2462  9.3719  8.4812  9.8576
      + expected[2, ] 32.0000 32.0000 32.0000 32.0000 32.0000 32.0000 32.0000
      - actual[3, ]    2.8477  3.0080  2.9235  2.6677  2.8257  3.0456  2.7825
      + expected[3, ] 32.0000 32.0000 32.0000 32.0000 32.0000 32.0000 32.0000
      
      `actual[[5]]` is a double vector (11.7355, 9.9637, 2.8477)
      `expected[[5]]` is an integer vector (32, 32, 32)
      
      `actual[[6]]` is a double vector (2.4296, 8.1957, 3.008)
      `expected[[6]]` is an integer vector (32, 32, 32)
      
      `actual[[7]]` is a double vector (14.2567, 7.8251, 2.9235)
      `expected[[7]]` is an integer vector (32, 32, 32)
      
      `actual[[8]]` is a double vector (35.2907, 10.2462, 2.6677)
      `expected[[8]]` is an integer vector (32, 32, 32)
      
      `actual[[9]]` is a double vector (6.5769, 9.3719, 2.8257)
      `expected[[9]]` is an integer vector (32, 32, 32)
      
      `actual[[10]]` is a double vector (11.8117, 8.4812, 3.0456)
      `expected[[10]]` is an integer vector (32, 32, 32)
      
      `actual[[11]]` is a double vector (18.749, 9.8576, 2.7825)
      `expected[[11]]` is an integer vector (32, 32, 32)

# e2e - tm_t_pp_laboratory: Selecting arind changes the table and does not throw validation errors.

    Code
      testthat::expect_identical(table_before, app_driver$
        get_active_module_table_output("lab_values_table", which = 2))
    Condition
      Error:
      ! Expected `table_before` to be identical to `app_driver$get_active_module_table_output(...)`.
      Differences:
      `actual[[5]]` is a double vector (11.7355, 9.9637, 2.8477)
      `expected[[5]]` is a character vector ('11.7355 YEARS', '9.9637 YEARS', '2.8477 YEARS')
      
      `actual[[6]]` is a double vector (2.4296, 8.1957, 3.008)
      `expected[[6]]` is a character vector ('2.4296 YEARS', '8.1957 YEARS', '3.008 YEARS')
      
      `actual[[7]]` is a double vector (14.2567, 7.8251, 2.9235)
      `expected[[7]]` is a character vector ('14.2567 YEARS', '7.8251 YEARS', '2.9235 YEARS')
      
      `actual[[8]]` is a double vector (35.2907, 10.2462, 2.6677)
      `expected[[8]]` is a character vector ('35.2907 YEARS', '10.2462 YEARS', '2.6677 YEARS')
      
      `actual[[9]]` is a double vector (6.5769, 9.3719, 2.8257)
      `expected[[9]]` is a character vector ('6.5769 YEARS', '9.3719 YEARS', '2.8257 YEARS')
      
      `actual[[10]]` is a double vector (11.8117, 8.4812, 3.0456)
      `expected[[10]]` is a character vector ('11.8117 YEARS', '8.4812 YEARS', '3.0456 YEARS')
      
      `actual[[11]]` is a double vector (18.749, 9.8576, 2.7825)
      `expected[[11]]` is a character vector ('18.749 YEARS', '9.8576 YEARS', '2.7825 YEARS')

