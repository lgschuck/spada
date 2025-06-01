# tests/testthat/test-order_rows_module.R

df_test <- iris
df_test[150, 1:5] <- NA

# test order rows - na last ---------------------------------------------------
test_that("Test order rows - na last - one variable ascending", {
  testServer(order_rows_server, {

    vars <- c('Species')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'last')

    session$setInputs(btn_order_rows = 1)

    df_reordered <- df_test[order(df_test[[vars]]), ]

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

test_that("Test order rows - na last - one variable descending", {
  testServer(order_rows_server, {

    vars <- c('Species')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'last')
    session$setInputs(vars_descending = vars)
    session$setInputs(btn_order_rows = 1)

    df_reordered <- df_test[order(df_test[[vars]], decreasing = TRUE), ]

    # setorderv keep rown.names as char
    row.names(df_reordered) <- as.character(rownames(df_reordered))

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

test_that("Test order rows - na last - two variables ascending", {
  testServer(order_rows_server, {

    vars <- c('Sepal.Length', 'Sepal.Width')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'last')

    session$setInputs(btn_order_rows = 1)

    df_reordered <- setorderv(df_test, vars, na.last = TRUE)

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

test_that("Test order rows - na last - two variables descending", {
  testServer(order_rows_server, {

    vars <- c('Sepal.Length', 'Sepal.Width')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'last')

    session$setInputs(btn_order_rows = 1)

    df_reordered <- setorderv(df_test, vars, c(-1, -1), na.last = TRUE)

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

# test order rows - na first --------------------------------------------------
test_that("Test order rows - na first - one variable ascending", {
  testServer(order_rows_server, {

    vars <- c('Species')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'first')

    session$setInputs(btn_order_rows = 1)

    df_reordered <- setorderv(df_test, vars, na.last = FALSE)

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

test_that("Test order rows - na first - one variable descending", {
  testServer(order_rows_server, {

    vars <- c('Species')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'first')
    session$setInputs(vars_descending = vars)
    session$setInputs(btn_order_rows = 1)

    df_reordered <- setorderv(df_test, vars, -1, na.last = FALSE)

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

test_that("Test order rows - na first - two variables ascending", {
  testServer(order_rows_server, {

    vars <- c('Sepal.Length', 'Petal.Width')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'first')

    session$setInputs(btn_order_rows = 1)

    df_reordered <- setorderv(df_test, vars, na.last = FALSE)

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})

test_that("Test order rows - na first - two variables descending", {
  testServer(order_rows_server, {

    vars <- c('Sepal.Length', 'Sepal.Width')

    session$userData$df <- reactiveValues(act = df_test)

    session$setInputs(vars_rows = vars)
    session$setInputs(radio_nas = 'first')
    session$setInputs(vars_descending = vars)
    session$setInputs(btn_order_rows = 1)

    df_reordered <- setorderv(df_test, vars, c(-1, -1), na.last = FALSE)

    expect_equal(df$df_active, df_reordered)
    expect_equal(session$userData$df$act, df_reordered)
  })
})
