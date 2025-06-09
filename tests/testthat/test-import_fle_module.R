# tests/testthat/test-import_file_module.R

# test import RDS -------------------------------------------------------------
test_that('Test import RDS', {
  testServer(import_file_server, {

    tmp <- tempfile(fileext = ".RDS")
    saveRDS(iris, tmp)

    # set inicial data
    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$dt[['mtcars']] <- mtcars

    session$setInputs(
      dataset_name = 'df_test',
      radio_file_ext = 'rds',
      file = list(datapath = tmp)
    )

    session$setInputs(btn_import = 1)

    expect_equal(data$data, iris |> as.data.table())
  })
})

# test import sav -------------------------------------------------------------
test_that('Test import Sav - all lines', {
  testServer(import_file_server, {

    tmp <- tempfile(fileext = ".sav")
    haven::write_sav(iris, tmp)

    # set inicial data
    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$dt[['mtcars']] <- mtcars

    session$setInputs(
      dataset_name = 'df_test',
      radio_file_ext = 'sav',
      file = list(datapath = tmp),
      sav_lines = 'all'
    )

    session$setInputs(btn_import = 1)

    # import data to test
    data_temp_test <- read_sav(tmp, .name_repair = make.names) |>
      as.data.table()
    data_temp_test[ , names(.SD) := lapply(.SD, as_factor), .SDcols = is.labelled]

    expect_equal(data$data, data_temp_test)
  })
})

test_that('Test import Sav - 10 lines', {
  testServer(import_file_server, {

    tmp <- tempfile(fileext = ".sav")
    haven::write_sav(iris, tmp)

    # set inicial data
    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$dt[['mtcars']] <- mtcars

    session$setInputs(
      dataset_name = 'df_test',
      radio_file_ext = 'sav',
      file = list(datapath = tmp),
      sav_lines = 'some',
      sav_n_lines = '10'
    )

    session$setInputs(btn_import = 1)

    # import data to test
    data_temp_test <- read_sav(tmp, n_max = 10, .name_repair = make.names) |>
      as.data.table()
    data_temp_test[ , names(.SD) := lapply(.SD, as_factor), .SDcols = is.labelled]

    expect_equal(data$data, data_temp_test)
  })
})

# test import csv -------------------------------------------------------------
test_that('Test import all lines - csv standard', {
  testServer(import_file_server, {

    tmp <- tempfile(fileext = ".csv")
    write.table(iris, tmp, sep = ',', dec = '.', row.names = F)

    # set inicial data
    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$dt[['mtcars']] <- mtcars

    session$setInputs(
      dataset_name = 'df_test',
      radio_file_ext = 'csv',
      file = list(datapath = tmp),
      radio_separator = ',',
      radio_decimal = '.',
      csv_lines = 'all',
      x_csv_header = TRUE
    )

    session$setInputs(btn_import = 1)

    data_temp <- fread(
        file = tmp,
        sep = ',',
        dec = '.',
        check.names = T,
        nrows = Inf,
        skip = 0,
        header = TRUE)

    expect_equal(data$data, data_temp)
  })
})

test_that('Test import all lines - csv 2 standard', {
  testServer(import_file_server, {

    tmp <- tempfile(fileext = ".csv")
    write.table(iris, tmp, sep = ';', dec = ',', row.names = F)

    # set inicial data
    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$dt[['mtcars']] <- mtcars

    session$setInputs(
      dataset_name = 'df_test',
      radio_file_ext = 'csv',
      file = list(datapath = tmp),
      radio_separator = ';',
      radio_decimal = ',',
      csv_lines = 'all',
      x_csv_header = TRUE
    )

    session$setInputs(btn_import = 1)

    data_temp <- fread(
      file = tmp,
      sep = ';',
      dec = ',',
      check.names = T,
      nrows = Inf,
      skip = 0,
      header = TRUE)

    expect_equal(data$data, data_temp)
  })
})
