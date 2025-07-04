# tests/testthat/test-filter_rows_module.R

df_test <- iris
df_test[150, 1:5] <- NA

# test filter rows - empty inputs ---------------------------------------------
test_that("Test filter rows - empty inputs - should not crash", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active, as.data.table(df_test))

  })
})

# test filter rows - one variable ---------------------------------------------
test_that("Test filter rows - one variable - equal operator", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = '==',
                      one_var_value = 'setosa')

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Species == 'setosa') |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not equal operator", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = '!=',
                      one_var_value = 'virginica')

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Species != 'virginica') |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - in operator", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'in',
                      one_var_value = c('virginica', 'setosa'))

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Species %in% c('virginica', 'setosa')) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not in operator", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'not_in',
                      one_var_value = c('virginica', 'setosa'))

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Species %notin% c('virginica', 'setosa')) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - is na", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'is_na')

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = is.na(Species)) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not na", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'not_na')

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = !is.na(Species)) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - grater", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Length',
                      one_var_operator = '>',
                      one_var_value = 5)

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Sepal.Length > 5) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - grater or equal", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Length',
                      one_var_operator = '>=',
                      one_var_value = 6)

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Sepal.Length >= 6) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - less", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Width',
                      one_var_operator = '<',
                      one_var_value = 3)

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Sepal.Width < 3) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - less or equal", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(df_test))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Width',
                      one_var_operator = '<=',
                      one_var_value = 3.3)

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 subset(df_test, subset = Sepal.Width <= 3.3) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - outlier", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(mtcars))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'outlier')

    session$setInputs(btn_filter = 1)
    expect_equal(df$df_active,
                 mtcars[Outlier(mtcars$hp, value = F), ] |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not outlier", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(mtcars))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'not_outlier')

    session$setInputs(btn_filter = 1)

    mtcars_dt <- mtcars |> as.data.table()
    expect_equal(df$df_active, mtcars_dt[!Outlier(mtcars$hp, value = F), ])

  })
})

test_that("Test filter rows - one variable - between", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(mtcars))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'between',
                      one_var_value = 90,
                      one_var_value2 = 100)

    session$setInputs(btn_filter = 1)

    mtcars_dt <- mtcars |> as.data.table()
    expect_equal(df$df_active, mtcars_dt[hp %between% c(90, 100), ])

  })
})

test_that("Test filter rows - one variable - not between", {
  testServer(filter_rows_server, {

    session$userData$df <- reactiveValues(act = as.data.table(mtcars))

    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'not_between',
                      one_var_value = 100,
                      one_var_value2 = 110)

    session$setInputs(btn_filter = 1)

    mtcars_dt <- mtcars |> as.data.table()
    expect_equal(df$df_active, mtcars_dt[!(hp %between% c(100, 110)), ])

  })
})
