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

