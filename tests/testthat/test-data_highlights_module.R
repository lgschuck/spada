# tests/testthat/test-data_highlights_module.R

# test data highlights - basic test -------------------------------------------
test_that("Test data highlights - basic test", {
  testServer(data_highlights_server, {

    df_test <- data.frame(
      num = c(1, 2, 3, NA, 5),
      char = c('A', 'B', 'A', 'B', NA),
      fact = factor(c('x', 'y', 'x', 'y', 'z')),
      date = as.Date(c('2020-01-01', '2020-01-02', NA, '2020-01-04', '2020-01-05')),
      all_na = rep(NA, 5),
      all_zeros = rep(0, 5),
      max_value = rep(Inf, 5),
      min_value = rep(-Inf, 5)
    )

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$df_info()[[session$userData$dt$act_name]]
    })

    expect_equal(output$var_num_vars, '4')
    expect_equal(output$var_char_vars, '1')
    expect_equal(output$var_factor_vars, '1')
    expect_equal(output$var_date_vars, '1')
    expect_equal(output$n_rows, '5')

    expect_equal(output$var_most_valid, 'fact')
    expect_equal(output$var_most_valid_n_valid, '5')

    expect_equal(output$var_most_unique, 'num')
    expect_equal(output$var_most_unique_n_unique, '5')

    expect_equal(output$var_most_zeros, 'all_zeros')
    expect_equal(output$var_most_zeros_n_zeros, '5')

    expect_equal(output$var_most_nas, 'all_na')
    expect_equal(output$var_most_nas_n, '5')

    expect_equal(output$var_max_value, 'max_value')
    expect_equal(output$max_value, 'Inf')

    expect_equal(output$var_min_value, 'min_value')
    expect_equal(output$min_value, '-Inf')

    expect_equal(output$var_biggest_size, 'fact')
    expect_equal(output$var_biggest_size_size,
                 factor(c('x', 'y', 'x', 'y', 'z')) |>
                   object.size() |>
                   as.character()
                 )

  })
})
