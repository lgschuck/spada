# tests/testthat/test-export_file_module.R

# test export csv -------------------------------------------------------------
test_that('Test export csv - test name', {

  testServer(export_file_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(
      file_name = 'iris_export',
      radio_format = 'csv',
      x_rownames = FALSE,
      radio_separator = ',',
      radio_decimal = '.',
      txt_na = 'NA',
      radio_scientific = 0
    )

    expect_equal(output$down_handler |> basename(), 'iris_export.csv')

  })
})

# test export RDS -------------------------------------------------------------
test_that('Test export RDS - test name', {

  testServer(export_file_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(
      file_name = 'iris_export',
      radio_format = 'RDS',
      checkbox_rds_compress = F
    )

    expect_equal(output$down_handler |> basename(), 'iris_export.RDS')

  })
})

# test export sav -------------------------------------------------------------
test_that('Test export sav - test name', {

  testServer(export_file_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(
      file_name = 'iris_export',
      radio_format = 'sav',
      radio_sav_compress = 'none'
    )

    expect_equal(output$down_handler |> basename(), 'iris_export.sav')

  })
})

# test export qs2 -------------------------------------------------------------
test_that('Test export qs2 - test name', {

  testServer(export_file_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(
      file_name = 'iris_export',
      radio_format = 'qs2',
      radio_sav_compress = 'none'
    )

    expect_equal(output$down_handler |> basename(), 'iris_export.qs2')

  })
})

# test export xlsx ------------------------------------------------------------
test_that('Test export xlsx - test name', {
  testServer(export_file_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(
      file_name = 'iris_export',
      radio_format = 'xlsx',
      radio_sav_compress = 'none'
    )

    expect_equal(output$down_handler |> basename(), 'iris_export.xlsx')

  })
})

test_that('Test export xlsx - test limit', {

  testServer(export_file_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df' = rep(1, 16385) |> as.list() |> as.data.table()),
      act_name = 'df'
    )

    session$setInputs(
      file_name = 'df_export',
      radio_format = 'xlsx',
      radio_sav_compress = 'none'
    )

    expect_equal(output$down_handler |> basename(), 'df_export.xlsx')

  })
})
