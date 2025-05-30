# tests/testthat/test-select_cols_module.R

# test drop columns -----------------------------------------------------------
test_that("Test drop one column", {
  testServer(select_cols_server, {

    session$userData$df <- reactiveValues(act = iris)

    session$setInputs(vars_sel = 'Species')
    session$setInputs(radio_var_sel = 'drop')
    session$setInputs(btn_sel = 1)

    expect_equal(df$df_active, iris |> subset(select = -Species))
    expect_equal(session$userData$df$act, iris |> subset(select = -Species))
  })
})

test_that("Test drop two column", {
  testServer(select_cols_server, {

    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_sel = c('hp', 'mpg'))
    session$setInputs(radio_var_sel = 'drop')
    session$setInputs(btn_sel = 1)

    expect_equal(df$df_active, mtcars |> subset(select = -c(hp, mpg)))
    expect_equal(session$userData$df$act, mtcars |> subset(select = -c(hp, mpg)))
  })
})

# test keep columns -----------------------------------------------------------
test_that("Test keep one column", {
  testServer(select_cols_server, {

    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_sel = 'hp')
    session$setInputs(radio_var_sel = 'keep')
    session$setInputs(btn_sel = 1)

    expect_equal(df$df_active, mtcars |> subset(select = hp))
    expect_equal(session$userData$df$act, mtcars |> subset(select = hp))
  })
})

test_that("Test keep two column", {
  testServer(select_cols_server, {

    session$userData$df <- reactiveValues(act = iris)

    session$setInputs(vars_sel = c('Species', 'Petal.Length'))
    session$setInputs(radio_var_sel = 'keep')
    session$setInputs(btn_sel = 1)

    expect_equal(df$df_active, iris |> subset(select = c(Species, Petal.Length)))
    expect_equal(session$userData$df$act,
                 iris |> subset(select = c(Species, Petal.Length)))
  })
})
