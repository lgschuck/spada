# tests/testthat/test-filter_rows_module.R

df_test <- iris
df_test[150, 1:5] <- NA

# test filter rows - empty inputs ---------------------------------------------
test_that("Test filter rows - empty inputs - should not crash", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session), as.data.table(df_test))

  })
})

# test filter rows - one variable ---------------------------------------------
test_that("Test filter rows - one variable - equal operator", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = '==',
                      one_var_value = 'setosa')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Species == 'setosa') |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not equal operator", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = '!=',
                      one_var_value = 'virginica')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Species != 'virginica') |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - in operator", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'in',
                      one_var_value = c('virginica', 'setosa'))

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Species %in% c('virginica', 'setosa')) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not in operator", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'not_in',
                      one_var_value = c('virginica', 'setosa'))

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Species %notin% c('virginica', 'setosa')) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - is na", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'is_na')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = is.na(Species)) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not na", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Species',
                      one_var_operator = 'not_na')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = !is.na(Species)) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - grater", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Length',
                      one_var_operator = '>',
                      one_var_value = 5)

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Sepal.Length > 5) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - grater or equal", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Length',
                      one_var_operator = '>=',
                      one_var_value = 6)

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Sepal.Length >= 6) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - less", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Width',
                      one_var_operator = '<',
                      one_var_value = 3)

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Sepal.Width < 3) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - less or equal", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = df_test |> as.data.table()),
      act_name = 'df_test'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'Sepal.Width',
                      one_var_operator = '<=',
                      one_var_value = 3.3)

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 subset(df_test, subset = Sepal.Width <= 3.3) |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - outlier", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'outlier')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session),
                 mtcars[Outlier(mtcars$hp, value = F), ] |>
                   as.data.table())

  })
})

test_that("Test filter rows - one variable - not outlier", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'not_outlier')

    session$setInputs(btn_filter = 1)

    mtcars_dt <- mtcars |> as.data.table()
    expect_equal(get_act_dt(session), mtcars_dt[!Outlier(mtcars$hp, value = F), ])

  })
})

test_that("Test filter rows - one variable - between", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'between',
                      one_var_value = 90,
                      one_var_value2 = 100)

    session$setInputs(btn_filter = 1)

    mtcars_dt <- mtcars |> as.data.table()
    expect_equal(get_act_dt(session), mtcars_dt[hp %between% c(90, 100), ])

  })
})

test_that("Test filter rows - one variable - not between", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'one',
                      one_var_sel = 'hp',
                      one_var_operator = 'not_between',
                      one_var_value = 100,
                      one_var_value2 = 110)

    session$setInputs(btn_filter = 1)

    mtcars_dt <- mtcars |> as.data.table()
    expect_equal(get_act_dt(session), mtcars_dt[!(hp %between% c(100, 110)), ])

  })
})

# test filter rows - two variables --------------------------------------------
test_that("Test filter rows - two variables - bigger", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'two',
                      two_var_sel1 = 'Sepal.Width',
                      two_var_operator = '>',
                      two_var_sel2 = 'Petal.Length')

    session$setInputs(btn_filter = 1)
    expect_equal(
      get_act_dt(session),
      iris |> as.data.table() |> subset(Sepal.Width > Petal.Length)
    )

  })
})



# test filter rows - sample ---------------------------------------------------
test_that("Test filter rows - sample - nrows", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'sample',
                      sample_type = 'rows',
                      n_rows = '15',
                      x_sample_replace = T)

    session$setInputs(btn_filter = 1)
    expect_equal(
      get_act_dt(session) |> nrow(),
      as.data.table(iris)[sample(1:nrow(iris), 15, replace = T), ] |> nrow()
    )

  })
})

test_that("Test filter rows - sample - percent", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'sample',
                      sample_type = 'percent',
                      sample_size = 10,
                      x_sample_replace = T)

    session$setInputs(btn_filter = 1)
    expect_equal(
      get_act_dt(session) |> nrow(),
      as.data.table(iris)[sample(1:nrow(iris), 10/100 * nrow(iris), replace = T), ] |> nrow()
    )

  })
})

# test filter rows - free -----------------------------------------------------
test_that("Test filter rows - free - allowed code", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'free',
                      txt_code_input = 'Sepal.Width <= Petal.Length'
                      )

    session$setInputs(btn_filter = 1)
    expect_equal(
      get_act_dt(session),
      iris |> as.data.table() |> subset(Sepal.Width <= Petal.Length)
    )

  })
})

test_that("Test filter rows - free - not allowed code - data stay unchanged", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'free',
                      txt_code_input = 'do.call(sum, list(1, 2))'
    )

    session$setInputs(btn_filter = 1)
    expect_equal(
      get_act_dt(session),
      iris |> as.data.table()
    )
  })
})

# test filter rows - from other dataset ---------------------------------------
test_that("Test filter rows - from other dataset - in", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table(),
                'df_test' = iris |> as.data.table() |> subset(Species == 'setosa')),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'dataset',
                      dt_1_var_sel = 'Species',
                      dt_var_operator = 'in',
                      dt_dt_sel = 'df_test',
                      dt_2_var_sel = 'Species')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session), iris |> as.data.table() |> subset(Species == 'setosa'))

  })
})

test_that("Test filter rows - from other dataset - not in", {
  testServer(filter_rows_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table(),
                'df_test' = iris |> as.data.table() |> subset(Species == 'setosa')),
      act_name = 'iris'
    )

    session$userData$dt$data_changed <- reactiveVal(0)
    session$setInputs(filter_type = 'dataset',
                      dt_1_var_sel = 'Species',
                      dt_var_operator = 'not_in',
                      dt_dt_sel = 'df_test',
                      dt_2_var_sel = 'Species')

    session$setInputs(btn_filter = 1)
    expect_equal(get_act_dt(session), iris |> as.data.table() |> subset(Species != 'setosa'))

  })
})

