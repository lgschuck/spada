# tests/testthat/test-select_cols_module.R

# test drop columns -----------------------------------------------------------
test_that("Test drop one column", {
  testServer(select_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(vars_sel = 'Species')
    session$setInputs(radio_var_sel = 'drop')
    session$setInputs(btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table() |> subset(select = -Species))
  })
})

test_that("Test drop two column", {
  testServer(select_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars),
      act_name = 'mtcars'
    )

    session$setInputs(vars_sel = c('hp', 'mpg'))
    session$setInputs(radio_var_sel = 'drop')
    session$setInputs(btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table() |> subset(select = -c(hp, mpg)))
  })
})

# test keep columns -----------------------------------------------------------
test_that("Test keep one column", {
  testServer(select_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars),
      act_name = 'mtcars'
    )

    session$setInputs(vars_sel = 'hp')
    session$setInputs(radio_var_sel = 'keep')
    session$setInputs(btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table() |> subset(select = hp))
  })
})

test_that("Test keep two column", {
  testServer(select_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
    )

    session$setInputs(vars_sel = c('Species', 'Petal.Length'))
    session$setInputs(radio_var_sel = 'keep')
    session$setInputs(btn_sel = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table() |> subset(select = c(Species, Petal.Length)))
  })
})
