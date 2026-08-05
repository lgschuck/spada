# tests/testthat/test-missing_module.R

dt <- data.table(
  Var1 = c(1, 1, NA, 10)
)

# test missing - identify - check messages ------------------------------------
test_that('Test missing - identify - check inputs', {
  testServer(missing_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg = function(text, ...) { last_msg <<- text },
      msg_error = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('dt' = dt),
      act_name = 'dt'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      var_sel_missing = NULL,
      txt_missing_name = 'Missing',
      btn_identify = 1
    )

    expect_equal(last_msg, 'Select at least one variable')

    session$setInputs(
      var_sel_missing = 'Var1',
      txt_missing_name = 'Var1',
      btn_identify = 2
    )

    expect_equal(last_msg, 'New names are not valid or already in use')

  })
})

# test missing - identify - check result --------------------------------------
test_that('Test missing - identify - check result', {
  testServer(missing_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg = function(text, ...) { last_msg <<- text },
      msg_error = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('dt' = dt),
      act_name = 'dt'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      var_sel_missing = 'Var1',
      txt_missing_name = 'Missing',
      btn_identify = 1
    )

    expect_equal(
      get_act_dt(session)$Missing,
      c('Not Missing', 'Not Missing', 'Missing', 'Not Missing') |> as.factor()
    )

  })
})






# test missing - replace - check inputs ---------------------------------------
test_that('Test missing - replace - check inputs', {
  testServer(missing_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg = function(text, ...) { last_msg <<- text },
      msg_error = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('dt' = dt),
      act_name = 'dt'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      var_sel_replace = 'Var1',
      sel_replace_method = 'constant',
      num_value = NULL,
      btn_replace = 1
    )

    expect_equal(last_msg, 'Inform a value')

  })
})


# test missing - replace - check result ---------------------------------------
test_that('Test missing - replace - check result', {
  testServer(missing_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg = function(text, ...) { last_msg <<- text },
      msg_error = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('dt' = dt),
      act_name = 'dt'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      var_sel_replace = 'Var1',
      sel_replace_method = 'constant',
      num_value = 2,
      btn_replace = 1
    )

    expect_equal(get_act_dt(session)$Var1, c(1, 1, 2, 10))

    session$userData$dt <- reactiveValues(
      dt = list('dt' = dt),
      act_name = 'dt'
    )

    session$setInputs(
      var_sel_replace = 'Var1',
      sel_replace_method = 'mean',
      btn_replace = 2
    )

    expect_equal(get_act_dt(session)$Var1, c(1, 1, 4, 10))

    session$userData$dt <- reactiveValues(
      dt = list('dt' = dt),
      act_name = 'dt'
    )

    session$setInputs(
      var_sel_replace = 'Var1',
      sel_replace_method = 'median',
      btn_replace = 3
    )

    expect_equal(get_act_dt(session)$Var1, c(1, 1, 1, 10))

  })
})
