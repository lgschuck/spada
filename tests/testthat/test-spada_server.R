# tests/testthat/test-spada_server.R

dfs <- list('df_iris' = iris, 'df_mtcars' = mtcars)

# test inputed datasets -------------------------------------------------------
test_that('Test inputed datasets', {
  testServer(spada_server(datasets = dfs), {
    expect_equal(session$userData$dt$dt[[1]], iris)
    expect_equal(session$userData$dt$dt[[2]], mtcars)
    expect_equal(names(session$userData$dt$dt), c('df_iris', 'df_mtcars'))
    expect_equal(session$userData$dt_names(), c('df_iris', 'df_mtcars'))
  })
})

# test active df --------------------------------------------------------------
test_that('Test active df', {
  testServer(spada_server(datasets = dfs), {
    expect_equal(session$userData$df$act, iris)
    expect_equal(session$userData$df$act_name, 'df_iris')
    expect_equal(session$userData$df$bkp, NULL)
  })
})

# test active df metadata -----------------------------------------------------
test_that('Test active df metadata', {
  testServer(spada_server(datasets = dfs), {
    expect_equal(session$userData$df$act_meta(), df_info(iris))
  })
})

# test change active df -------------------------------------------------------
test_that('Test change active df', {
  testServer(spada_server(datasets = dfs), {

    session$setInputs(pD_data_sel_df = 'df_mtcars')

    session$setInputs(pD_data_btn_active = 1)

    expect_equal(session$userData$df$act_name, 'df_mtcars')
    expect_equal(session$userData$df$act, mtcars)

  })
})

# test rename active df -------------------------------------------------------
test_that('Test rename active df', {
  testServer(spada_server(datasets = dfs), {
    session$setInputs(pD_data_sel_df = 'df_iris')
    session$setInputs(pD_data_txt_new_name = 'new_iris')

    session$setInputs(pD_data_btn_new_name = 1)

    expect_equal(session$userData$df$act_name, 'new_iris')
  })
})

test_that('Test rename active df - invalid name', {
  testServer(spada_server(datasets = dfs), {
    session$setInputs(pD_data_sel_df = 'df_iris')
    session$setInputs(pD_data_txt_new_name = 'new iris')

    session$setInputs(pD_data_btn_new_name = 1)

    expect_equal(session$userData$df$act_name, 'df_iris')
  })
})

# test copy df --------------------------------------------------------------
test_that('Test copy df', {
  testServer(spada_server(datasets = dfs), {

    session$setInputs(pD_data_txt_new_name = 'df_iris2')

    session$setInputs(pD_data_btn_copy_dataset = 1)

    expect_equal(session$userData$dt_names(), c('df_iris', 'df_mtcars', 'df_iris2'))
    expect_equal(session$userData$dt$dt[[3]], iris)
    expect_equal(session$userData$dt$dt |> length(), 3)
  })
})

# test delete df --------------------------------------------------------------
test_that('Test delete non active df', {
  testServer(spada_server(datasets = dfs), {
    session$setInputs(pD_data_sel_df = 'df_mtcars')

    session$setInputs(pD_data_btn_delete_dataset = 1)

    expect_equal(session$userData$dt_names(), 'df_iris')
    expect_equal(session$userData$dt$dt[[1]], iris)
    expect_equal(session$userData$dt$dt |> length(), 1)
  })
})

test_that('Test delete active df', {
  testServer(spada_server(datasets = dfs), {
    session$setInputs(pD_data_sel_df = 'df_iris')

    session$setInputs(pD_data_btn_delete_dataset = 1)

    expect_equal(session$userData$df$act_name, 'df_iris')
    expect_equal(session$userData$df$act, iris)
  })
})

