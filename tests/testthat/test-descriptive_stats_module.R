# tests/testthat/test-descriptive_stats_module.R

var1 <- 'hp'

# test ddescriptiveesc stats - reactives --------------------------------------
test_that('Test desc stats - reactives', {

  testServer(descriptive_stats_server, {

    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(
      sel_var = var1,
      table_digits = 9,
      xg_central_tendency = 'mean',
      xg_dispersion = '',
      xg_shape = '',
      btn_stats = 1
    )

    expect_true(df |> is.reactive())
    expect_equal(df(), mtcars)

    expect_true(df_stats |> is.reactive())
    expect_equal(df_stats(), mtcars[var1])

    expect_true(desc_stats |> is.reactive())
    expect_true(desc_stats() |> is.list())
    expect_true((desc_stats() |> length()) == 1)
  })
})

# test descriptive stats - desc_stats mean 1 var ------------------------------
test_that('Test descriptive stats - desc_stats mean 1 var', {

  testServer(descriptive_stats_server, {

    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(
      sel_var = var1,
      table_digits = 9,
      xg_central_tendency = 'mean',
      xg_dispersion = '',
      xg_shape = '',
      btn_stats = 1
    )

    expect_equal(df(), mtcars)
    expect_equal(df_stats(), mtcars[var1])
    expect_true(desc_stats |> is.reactive())
    expect_true(desc_stats() |> is.list())
    expect_true((desc_stats() |> length()) == 1)

    mean_hp <- mean(mtcars[, var1]) |> f_num(dig = 9)
    names(mean_hp) = var1
    expect_equal(desc_stats(), list('Mean' = mean_hp))

  })
})

# test descriptive stats - gt stats -------------------------------------------
test_that('Test descriptive stats - gt stats', {

  testServer(descriptive_stats_server, {

    session$userData$df <- reactiveValues(act = mtcars)

    session$setInputs(
      sel_var = var1,
      table_digits = 9,
      xg_central_tendency = 'mean',
      xg_dispersion = '',
      xg_shape = '',
      btn_stats = 1
    )

    expect_true(gt_stats |> is.reactive())
    expect_equal(gt_stats() |> class(), c('gt_tbl', 'list'))

  })
})
