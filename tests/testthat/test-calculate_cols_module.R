# tests/testthat/test-calculate_cols_module.R

iris_dt <- iris |> as.data.table()

# test calculate cols - apply function ----------------------------------------
test_that("Test calculate cols - sum numeric", {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'sum',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
      )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]]$new_var[1],
                 iris$Sepal.Width |> sum())
  })
})

test_that("Test calculate cols - mean numeric - groupby", {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$setInputs(
      vars_sel = 'Sepal.Width',
      fun = 'mean',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = 'Species'
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(
      df$df_active,
      iris_dt[, new_var := mean(Sepal.Width), by = 'Species']
    )
  })
})

test_that("Test calculate cols - factor", {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$setInputs(
      vars_sel = 'Species',
      fun = 'is.factor',
      txt_new_name_fun = 'new_var',
      vars_groupby_fun = NULL
    )

    session$setInputs(btn_apply_fun = 1)

    expect_equal(df$df_active, iris_dt[, new_var := is.factor(Species)])
  })
})

# test calculate cols - freehand ----------------------------------------------
test_that("Test calculate cols - freehand", {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$setInputs(
      txt_new_name_free = 'new_var',
      vars_groupby_free = NULL,
      txt_code_input = '10'
    )

    session$setInputs(btn_apply_free = 1)

    expect_equal(df$df_active, iris_dt[, new_var := 10 ])
  })
})

test_that("Test calculate cols - freehand - groupby", {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$setInputs(
      txt_new_name_free = 'new_var',
      vars_groupby_free = 'Species',
      txt_code_input = 'mean(Sepal.Length)'
    )

    session$setInputs(btn_apply_free = 1)

    expect_equal(
      df$df_active,
      iris_dt[, new_var := mean(Sepal.Length), by = 'Species' ]
    )
  })
})

test_that("Test calculate cols - freehand - fifelse", {
  testServer(calculate_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$setInputs(
      txt_new_name_free = 'new_var',
      vars_groupby_free = NULL,
      txt_code_input = "fifelse(Species == 'setosa', 1, 2)"
    )

    session$setInputs(btn_apply_free = 1)

    expect_equal(
      df$df_active,
      iris_dt[, new_var := fifelse(Species == 'setosa', 1, 2) ]
    )
  })
})
