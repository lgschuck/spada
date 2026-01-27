# tests/testthat/test-data_module.R

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

# test make active dt ---------------------------------------------------------
test_that('Test data - make active dt', {
  testServer(data_server, {

    session$userData$dt_names <- reactive(dfs |> names())
    session$userData$dt$dt <- dfs
    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      sel_dt = 'df_mtcars',
      btn_active = 1
    )

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      mtcars |> as.data.table()
    )
  })
})

# test copy dt ----------------------------------------------------------------
test_that('Test data - copy dt', {
  testServer(data_server, {

    session$userData$dt_names <- reactive(dfs |> names())
    session$userData$dt$dt <- dfs
    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      sel_dt = 'df_mtcars',
      txt_new_name = 'copy',
      btn_copy_dt = 1
    )

    expect_equal(
      session$userData$dt$dt[['copy']],
      mtcars |> as.data.table()
    )
  })
})

# test del dt -----------------------------------------------------------------
test_that('Test data - del dt', {
  testServer(data_server, {

    session$userData$dt_names <- reactive(dfs |> names())
    session$userData$dt$dt <- dfs
    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      sel_dt = 'df_mtcars',
      btn_del_dt = 1
    )

    expect_equal(session$userData$dt$dt |> names(), 'df_iris')
    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris |> as.data.table()
    )
  })
})

test_that('Test try delete active df', {
  testServer(data_server, {

    session$userData$dt_names <- reactive(dfs |> names())
    session$userData$dt$dt <- dfs
    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      sel_dt = 'df_iris',
      btn_del_dt = 1
    )

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris |> as.data.table()
    )
  })
})

# test rename dt --------------------------------------------------------------
test_that('Test data - del dt', {
  testServer(data_server, {

    session$userData$dt_names <- reactive(dfs |> names())
    session$userData$dt$dt <- dfs
    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      txt_new_name = 'dataset',
      sel_dt = 'df_mtcars',
      btn_new_name = 1
    )

    expect_equal(session$userData$dt$dt |> names(), c('df_iris', 'dataset'))
    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris |> as.data.table()
    )
    expect_equal(
      session$userData$dt$dt[['dataset']],
      mtcars |> as.data.table()
    )
  })
})

test_that('Test rename active df - invalid name', {
  testServer(data_server,{

    session$userData$dt_names <- reactive(dfs |> names())
    session$userData$dt$dt <- dfs
    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      txt_new_name = 'new dataset',
      sel_dt = 'df_mtcars',
      btn_new_name = 1
    )

    expect_equal(session$userData$dt$act_name, 'df_iris')
  })
})
