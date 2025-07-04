# tests/testthat/test-order_cols_module.R

# test order columns - before -------------------------------------------------
test_that("Test order one column - before all", {
  testServer(order_cols_server, {

    vars <- c('wt')
    var1 <- names(mtcars)[1]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'before')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .before = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

test_that("Test order two column - before all", {
  testServer(order_cols_server, {

    vars <- c('wt', 'hp')
    var1 <- names(mtcars)[1]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'before')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .before = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

test_that("Test order one column - before especific", {
  testServer(order_cols_server, {

    vars <- c('wt')
    var1 <- names(mtcars)[4]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'before')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .before = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

test_that("Test order two columns - before especific", {
  testServer(order_cols_server, {

    vars <- c('wt','mpg')
    var1 <- names(mtcars)[4]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'before')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .before = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

# test order columns - after -------------------------------------------------
test_that("Test order one column - after all", {
  testServer(order_cols_server, {

    vars <- c('wt')
    var1 <- names(mtcars)[length(mtcars)]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'after')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .after = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

test_that("Test order two columns - after all", {
  testServer(order_cols_server, {

    vars <- c('wt', 'drat')
    var1 <- names(mtcars)[length(mtcars)]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'after')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .after = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

test_that("Test order one column - after specific", {
  testServer(order_cols_server, {

    vars <- c('am')
    var1 <- names(mtcars)[8]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'after')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .after = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})

test_that("Test order two columns - after specific", {
  testServer(order_cols_server, {

    vars <- c('am', 'carb')
    var1 <- names(mtcars)[2]
    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(vars_cols = vars)
    session$setInputs(vars_rest = var1)
    session$setInputs(radio_cols = 'after')
    session$setInputs(btn_order_cols = 1)

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .after = tidyselect::all_of(var1))

    expect_equal(df$df_active, mtcars_reordered)
    expect_equal(session$userData$df$act, mtcars_reordered)
  })
})
