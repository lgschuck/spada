# tests/testthat/test-data_bkp_module.R

dfs <- list('df_iris' = iris |> as.data.table(),
            'df_mtcars' = mtcars |> as.data.table())

# test create bkp -------------------------------------------------------------
test_that('Test data bkp - create bkp', {
  testServer(data_bkp_server, {

    session$userData$dt_names <- reactive('iris')

    session$userData$dt <- reactiveValues(
      dt = list('iris' = dfs[['df_iris']]),
      act_name = 'iris'
    )

    expect_null(session$userData$dt$bkp)
    session$setInputs(btn_bkp = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      session$userData$dt$bkp
    )
  })
})

# test clear bkp --------------------------------------------------------------
test_that('Test data bkp - clear bkp', {
  testServer(data_bkp_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt <- reactiveValues(
      dt = list('iris' = dfs[['df_iris']]),
      act_name = 'iris'
    )

    session$setInputs(btn_bkp = 1)
    expect_true(!is.null(session$userData$dt$bkp))

    session$setInputs(btn_clear_bkp = 1)

    expect_null(session$userData$dt$bkp)
  })
})

# test restore bkp --------------------------------------------------------------
test_that('Test data bkp - restore bkp', {
  testServer(data_bkp_server, {

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive('iris')
    session$userData$dt <- reactiveValues(
      dt = list('iris' = dfs[['df_iris']]),
      act_name = 'iris'
    )

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

    session$userData$dt <- reactiveValues(
      dt = list('iris' = dfs[['df_iris']]),
      act_name = 'iris',
      bkp0 = dfs[['df_iris']]
    )

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

# test messages ---------------------------------------------------------------
test_that('Test data bkp - check messages', {
  testServer(data_bkp_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      bkp = NULL
    )

    session$setInputs(btn_restore = 1)
    expect_equal(last_msg, 'No backup to restore')

    session$setInputs(btn_clear_bkp = 1)
    expect_equal(last_msg, 'No backup to clear')
  })
})
