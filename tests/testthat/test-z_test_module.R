# tests/testthat/test-z_test_module.R

# test z test module - variable selection -------------------------------------
test_that('Z Test module - variable selection', {
  testServer(z_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$df_info()[[session$userData$dt$act_name]]
    })

    session$setInputs(sel_var = 'Petal.Length')

    expect_equal(var_analysis(), setdiff(names(iris), 'Species'))
    expect_equal(var(), iris$Petal.Length)
    expect_equal(sample_mean(), mean(iris$Petal.Length))
    expect_equal(sample_sd(), sd(iris$Petal.Length))
  })
})

# test z test - results -------------------------------------------------------
test_that('Z Test module - test results', {
  testServer(z_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$df_info()[[session$userData$dt$act_name]]
    })

    session$setInputs(
      sel_var = 'Petal.Length',
      confidence = 90,
      radio_alternative = 'two.sided',
      mu = 5,
      sd = 2,
      btn_run_test = 1
    )

    z1 <- ZTest(iris$Petal.Length,
                alternative = 'two.sided',
                mu = 5,
                sd_pop = 2,
                conf.level = 0.9)

    expect_s3_class(ztest_results_gt(), 'gt_tbl')
    expect_equal(var(), iris$Petal.Length)
    expect_equal(ztest$results |> nrow(), 11)
    expect_equal(ztest$results |> ncol(), 2)
    expect_equal(ztest$results |> names(), c('values', 'results'))
    expect_equal(ztest$results$values[1], z1$statistic |> as.character())
    expect_equal(ztest$results$values[2], z1$parameter |> as.character())
    expect_equal(ztest$results$values[3], z1$p.value |> as.character())
    expect_equal(ztest$results$values[4], z1$conf.int[1] |> as.character())
    expect_equal(ztest$results$values[5], z1$conf.int[2] |> as.character())
    expect_equal(ztest$results$values[6], z1$estimate |> as.character())
    expect_equal(ztest$results$values[7], z1$null.value |> as.character())
    expect_equal(ztest$results$values[8], z1$stderr |> as.character())
    expect_equal(ztest$results$values[9], z1$alternative |> as.character())
    expect_equal(ztest$results$values[10], z1$method |> as.character())
    expect_equal('Petal.Length', ztest$results$values[11])
  })
})

# test z test - histogram -----------------------------------------------------
test_that('Z Test module - histogram', {
  testServer(z_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$conf <- reactiveValues(
      plot_fill_color = '#229999',
      plot_line_color = '#44aa44'
    )

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$df_info()[[session$userData$dt$act_name]]
    })

    session$setInputs(
      sel_var = 'Petal.Length',
      btn_hist = 1
    )

    expect_s3_class(ztest_hist(), c('gg', 'ggplot'))
  })
})
