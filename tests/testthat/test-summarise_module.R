# tests/testthat/test-summarise_module.R

dt <- iris |> as.data.table()

# test summarise - distinct ---------------------------------------------------
test_that('Test summarise - new dt - distinct function', {
  testServer(summarise_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dt
    session$userData$dt$act_name <- 'iris'

    session$setInputs(
      vars_sel = 'Petal.Length',
      fun_sel = 'distinct',
      radio_overwrite = 'new',
      txt_new_dt_name = 'new_dt',
      btn_summarise = 1
    )

    expect_equal(
      session$userData$dt$dt[['new_dt']],
      dt[, .SD, .SDcols = 'Petal.Length'] |> unique()
    )
  })
})

test_that('Test summarise - overwrite dt - distinct function', {
  testServer(summarise_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dt
    session$userData$dt$act_name <- 'iris'
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Petal.Length',
      fun_sel = 'distinct',
      radio_overwrite = 'overwrite',
      btn_summarise = 1
    )

    expect_equal(
      session$userData$dt$dt[['iris']],
      dt[, .SD, .SDcols = 'Petal.Length'] |> unique()
    )
  })
})

# test input check messages ---------------------------------------------------
test_that('Test sumamrise - check inputs', {
  testServer(summarise_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dt
    session$userData$dt$act_name <- 'iris'

    session$setInputs(vars_sel = NULL,btn_summarise = 1)

    expect_equal(last_msg, 'Insert at least one variable')

    session$setInputs(
      vars_sel = 'Species',
      radio_overwrite = 'new',
      txt_new_dt_name = 123,
      btn_summarise = 2
    )

    expect_equal(last_msg, 'New name is not valid or already in use')
  })
})
