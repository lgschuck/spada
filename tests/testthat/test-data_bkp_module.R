# tests/testthat/test-data_bkp_module.R

dfs <- list('df_iris' = iris |> as.data.table(),
            'df_mtcars' = mtcars |> as.data.table())

temp_dir <- tempdir()

start_conf <- list(
  'conf_dir' = paste0(temp_dir, '\\conf'),
  'data_dir' = paste0(temp_dir, '\\data'),
  'theme' = 'spada_theme',
  'file_size' = 1000,
  'restore_session' = 'never',
  'save_session' = 'ask',
  'restore_data_status' = 0,
  'restore_output_status' = 0,
  'restore_status' = NULL,
  'plot_fill_color' = plot_fill_color,
  'plot_line_color' = plot_line_color
)

# test create bkp -------------------------------------------------------------
test_that('Test data bkp - create bkp', {
  testServer(data_bkp_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dfs[['df_iris']]
    session$userData$dt$act_name <- 'iris'

    session$setInputs(btn_bkp = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      session$userData$dt$bkp
    )
  })
})

test_that('Test data bkp - no bkp created', {
  testServer(data_bkp_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dfs[['df_iris']]
    session$userData$dt$act_name <- 'iris'

    expect_null(session$userData$dt$bkp)
  })
})

# test clear bkp --------------------------------------------------------------
test_that('Test data bkp - clear bkp', {
  testServer(data_bkp_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dfs[['df_iris']]
    session$userData$dt$act_name <- 'iris'

    session$setInputs(btn_bkp = 1)
    session$setInputs(btn_clear_bkp = 1)

    expect_null(session$userData$dt$bkp)
  })
})

# test restore bkp --------------------------------------------------------------
test_that('Test data bkp - restore bkp', {
  testServer(data_bkp_server, {

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dfs[['df_iris']]
    session$userData$dt$act_name <- 'iris'

    session$setInputs(btn_bkp = 1)

    session$userData$dt$dt[['iris']] <- iris |> subset(select = 'Species')

    session$setInputs(btn_restore = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris |> as.data.table()
    )
  })
})

# test reset bkp --------------------------------------------------------------
test_that('Test data bkp - reset bkp', {
  testServer(data_bkp_server, {

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dfs[['df_iris']]
    session$userData$dt$act_name <- 'iris'
    session$userData$dt$bkp0 <- dfs[['df_iris']]

    session$setInputs(btn_bkp = 1)

    session$userData$dt$dt[['iris']] <- iris |> subset(select = 'Species')

    session$setInputs(btn_restore = 1)

    session$userData$dt$dt[['iris']] <- iris |> subset(select = 'Petal.Length')

    session$setInputs(btn_reset = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris |> as.data.table()
    )
  })
})
