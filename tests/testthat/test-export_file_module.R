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
