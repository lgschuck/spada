# tests/testthat/test-select_cols_module.R

# test drop columns -----------------------------------------------------------
test_that('Test drop one column', {
  testServer(select_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      remove_running_modal = function() { last_msg <<- 'Remove modal' }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(vars_sel = 'Species',
                      radio_var_sel = 'drop',
                      btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table() |> subset(select = -Species))

    expect_equal(last_msg, 'Remove modal')
  })
})

test_that('Test drop two column', {
  testServer(select_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      remove_running_modal = function() { last_msg <<- 'Remove modal' }
    )

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(vars_sel = c('hp', 'mpg'),
                      radio_var_sel = 'drop',
                      btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table() |> subset(select = -c(hp, mpg)))
    expect_equal(last_msg, 'Remove modal')
  })
})

# test keep columns -----------------------------------------------------------
test_that('Test keep one column', {
  testServer(select_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      remove_running_modal = function() { last_msg <<- 'Remove modal' }
    )

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(vars_sel = 'hp',
                      radio_var_sel = 'keep',
                      btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table() |> subset(select = hp))
    expect_equal(last_msg, 'Remove modal')
  })
})

test_that('Test keep two column', {
  testServer(select_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      remove_running_modal = function() { last_msg <<- 'Remove modal' }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(vars_sel = c('Species', 'Petal.Length'),
                      radio_var_sel = 'keep',
                      btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table() |> subset(select = c(Species, Petal.Length)))
    expect_equal(last_msg, 'Remove modal')
  })
})

# test check messages ---------------------------------------------------------
test_that('Select cols - check inputs', {
  testServer(select_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(btn_sel = 1)

    expect_equal(last_msg, 'Select at least one variable')

    session$setInputs(radio_var_sel = 'drop', vars_sel = names(iris), btn_sel = 2)

    expect_equal(last_msg, 'Leave at least 1 variable')

  })
})
