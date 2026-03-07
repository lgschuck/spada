# tests/testthat/test-calculate_cols_module.R

iris_dt <- iris |> as.data.table()

# test calculate cols - apply function ----------------------------------------
test_that('Test calculate cols - sum numeric', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'sum_na',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
      )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]]$new_var[1],
                 iris$Sepal.Width |> sum())
  })
})

test_that('Test calculate cols - mean numeric - groupby', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'mean_na',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = 'Species'
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris_dt[, new_var := mean(Sepal.Width), by = 'Species']
    )
  })
})

test_that('Test calculate cols - factor', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Species',
      fun = 'is.factor',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris_dt[, new_var := is.factor(Species)])
  })
})

# test calculate cols - freehand ----------------------------------------------
test_that('Test calculate cols - freehand', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      txt_new_name_free = 'new_var',
      vars_groupby_free = NULL,
      txt_code_input = '10'
    )

    session$setInputs(btn_apply_free = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris_dt[, new_var := 10 ])
  })
})

test_that('Test calculate cols - freehand - groupby', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      txt_new_name_free = 'new_var',
      vars_groupby_free = 'Species',
      txt_code_input = 'mean(Sepal.Length)'
    )

    session$setInputs(btn_apply_free = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris_dt[, new_var := mean(Sepal.Length), by = 'Species' ]
    )
  })
})

test_that('Test calculate cols - freehand - fifelse', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      txt_new_name_free = 'new_var',
      vars_groupby_free = NULL,
      txt_code_input = "fifelse(Species == 'setosa', 1, 2)"
    )

    session$setInputs(btn_apply_free = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      iris_dt[, new_var := fifelse(Species == 'setosa', 1, 2) ]
    )
  })
})

# test calculate cols - not allowed function ----------------------------------
test_that('Test calculate cols - not allowed function', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'not_allowed',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]]$Sepal.Width,
                 iris$Sepal.Width)
  })
})

# test calculate cols - apply fun - not allowed -------------------------------
test_that('Test calculate cols - apply fun - not allowed function', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'not_allowed',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]]$Sepal.Width,
                 iris$Sepal.Width)
  })
})

test_that('Test calculate cols - apply fun - dangerous function', {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'assign',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]]$Sepal.Width,
                 iris$Sepal.Width)
  })
})

# test check messages ---------------------------------------------------------
test_that('Calculate cols - apply fun - check inputs', {
  testServer(calculate_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    # vars_sel NULL
    session$setInputs(vars_sel = NULL, btn_apply_fun = 1)
    expect_equal(last_msg, 'Select at least one variable')

    # fun NULL
    session$setInputs(vars_sel = 'Species', fun = NULL, btn_apply_fun = 2)
    expect_equal(last_msg, 'Select a function')

    # dangerous function
    session$setInputs(vars_sel = 'Species', fun = 'unlink', btn_apply_fun = 3)
    expect_equal(last_msg, 'Function is not allowed')

    # new var name invalid
    session$setInputs(vars_sel = 'Species', fun = 'mean_na',
                      txt_new_name_fun = 123, btn_apply_fun = 4)
    expect_equal(last_msg, 'New name is not valid or already in use')

    # new var name already in use
    session$setInputs(vars_sel = 'Species', fun = 'mean_na',
                      txt_new_name_fun = 'Species', btn_apply_fun = 5)
    expect_equal(last_msg, 'New name is not valid or already in use')

    # apply function ok
    session$setInputs(vars_sel = 'Species', fun = 'is.factor',
                      txt_new_name_fun = 'Species2', btn_apply_fun = 6)
    expect_equal(last_msg, 'Apply function: OK')

  })
})

test_that('Calculate cols - free hand - check inputs', {
  testServer(calculate_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    # error parse code
    session$setInputs(txt_code_input = '///', txt_new_name_free = 'Species2',
                      btn_apply_free = 1)
    expect_equal(last_msg, 'Error to validate expression. Check code')

    # not allowed
    session$setInputs(txt_code_input = 'unlink(x)', txt_new_name_free = 'Species2',
                      btn_apply_free = 2)
    expect_equal(last_msg, 'Some operations are not allowed')


    # varible not present
    session$setInputs(txt_code_input = 'mean(var1)',
                      txt_new_name_free = 'Species2',
                      btn_apply_free = 3)
    expect_equal(last_msg, 'Some variables are not present in the dataset')

    # new var name already in use
    session$setInputs(txt_code_input = 'mean(Sepal.Length)',
                      txt_new_name_free = 'Species_mean 123',
                      vars_groupby_free = 'Species',
                      btn_apply_free = 4)
    expect_equal(last_msg, 'New name is not valid or already in use')

    # error - log of factor
    session$setInputs(txt_code_input = 'log(Species)',
                      txt_new_name_free = 'Species_log',
                      btn_apply_free = 5)
    expect_equal(last_msg, 'Error in expression. Check code')

    # calculate ok
    session$setInputs(txt_code_input = 'mean(Sepal.Length)',
                      txt_new_name_free = 'Species_mean',
                      vars_groupby_free = 'Species',
                      btn_apply_free = 6)
    expect_equal(last_msg, 'Calculate new var: OK')

  })
})
