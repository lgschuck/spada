# tests/testthat/test-spada_server.R

dfs <- list('df_iris' = iris, 'df_mtcars' = mtcars)

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

# test inputed datasets -------------------------------------------------------
test_that('Test inputed datasets', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$dt[[1]], iris |> as.data.table())
    expect_equal(session$userData$dt$dt[[2]], mtcars |> as.data.table())
    expect_equal(names(session$userData$dt$dt), c('df_iris', 'df_mtcars'))
    expect_equal(session$userData$dt_names(), c('df_iris', 'df_mtcars'))
  })
})

# test check datasets classes -------------------------------------------------
test_that('Test inputed datasets - classes', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$dt[[1]] |> class(), c('data.table', 'data.frame'))
    expect_equal(session$userData$dt$dt[[2]] |> class(), c('data.table', 'data.frame'))
  })
})

# test active df --------------------------------------------------------------
test_that('Test active df', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$act_name, 'df_iris')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 dfs[['df_iris']] |> as.data.table())
    expect_equal(session$userData$dt$bkp, NULL)
  })
})

# test active df metadata -----------------------------------------------------
test_that('Test active df metadata', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$act_meta(), df_info(iris))
  })
})

# test metadata - df info -----------------------------------------------------
test_that('Test metadata - df_info', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    expect_equal(df_info(session$userData$dt$dt[[session$userData$dt$act_name]]) |> nrow(), 5)
    expect_equal(df_info(session$userData$dt$dt[[session$userData$dt$act_name]]), df_info(iris))
  })
})

# test metadata - df info -----------------------------------------------------
test_that('Test metadata - gt class', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$df$act_name <- 'df_mtcars'

    expect_true('gt_tbl' %in% class(session$userData$dt$act_meta() |>
                                      gt_info(df_name = names(dfs)[2])))

  })
})

# test metadata - gt metadata -------------------------------------------------
test_that('Test metadata - gt metadata', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    mtcars_meta <- mtcars |> df_info() |> gt_info(df_name = 'df_mtcars')
    session$userData$dt$act_name <- 'df_mtcars'

    gt_temp <- session$userData$dt$act_meta() |> gt_info(df_name = names(dfs)[2])

    expect_equal(gt_temp$`_data`, mtcars_meta$`_data`)
    expect_equal(gt_temp$`_data` |> nrow(), 11)
    expect_equal(gt_temp$`_data`$var, names(mtcars))
    expect_equal(gt_temp$`_data`$type, sapply(mtcars, typeof) |> unname())
    expect_equal(gt_temp$`_data`$class, sapply(mtcars, class) |> unname())
  })
})

# test change active df -------------------------------------------------------
test_that('Test change active df', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$setInputs(pD_data_sel_df = 'df_mtcars')

    session$setInputs(pD_data_btn_active = 1)

    expect_equal(session$userData$dt$act_name, 'df_mtcars')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table())

  })
})

# test rename active df -------------------------------------------------------
test_that('Test rename active df', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    session$setInputs(pD_data_sel_df = 'df_iris')
    session$setInputs(pD_data_txt_new_name = 'new_iris')

    session$setInputs(pD_data_btn_new_name = 1)

    expect_equal(session$userData$dt$act_name, 'new_iris')
  })
})

test_that('Test rename active df - invalid name', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    session$setInputs(pD_data_sel_df = 'df_iris')
    session$setInputs(pD_data_txt_new_name = 'new iris')

    session$setInputs(pD_data_btn_new_name = 1)

    expect_equal(session$userData$dt$act_name, 'df_iris')
  })
})

# test copy df --------------------------------------------------------------
test_that('Test copy df', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$setInputs(pD_data_txt_new_name = 'df_iris2')
    session$setInputs(pD_data_sel_df = 'df_iris')
    session$setInputs(pD_data_btn_copy_dataset = 1)

    expect_equal(session$userData$dt_names(), c('df_iris', 'df_mtcars', 'df_iris2'))
    expect_equal(session$userData$dt$dt[[3]], iris |> as.data.table())
    expect_equal(session$userData$dt$dt |> length(), 3)
  })
})

# test delete df --------------------------------------------------------------
test_that('Test delete non active df', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    session$setInputs(pD_data_sel_df = 'df_mtcars')

    session$setInputs(pD_data_btn_delete_dataset = 1)

    expect_equal(session$userData$dt_names(), 'df_iris')
    expect_equal(session$userData$dt$dt[[1]], iris |> as.data.table())
    expect_equal(session$userData$dt$dt |> length(), 1)
  })
})

test_that('Test delete active df', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    session$setInputs(pD_data_sel_df = 'df_iris')

    session$setInputs(pD_data_btn_delete_dataset = 1)

    expect_equal(session$userData$dt$act_name, 'df_iris')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table())
  })
})
