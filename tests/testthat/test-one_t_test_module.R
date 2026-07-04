# tests/testthat/test-one_t_test_module.R

# test one t test module - variable selection -------------------------------------
test_that('One-sample t Test module - variable selection', {
  testServer(one_t_test_server, {
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
  })
})

# test one t test - results ---------------------------------------------------
test_that('One-sample t Test module - test results', {
  testServer(one_t_test_server, {
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
      btn_run_test = 1
    )

    t1 <- t.test(iris$Petal.Length,
                alternative = 'two.sided',
                mu = 5,
                conf.level = 0.9)

    expect_s3_class(ttest_results_gt(), 'gt_tbl')
    expect_equal(var(), iris$Petal.Length)
    expect_equal(ttest$results |> nrow(), 11)
    expect_equal(ttest$results |> ncol(), 2)
    expect_equal(ttest$results |> names(), c('values', 'results'))
    expect_equal(ttest$results$values[1], t1$statistic |> as.character())
    expect_equal(ttest$results$values[2], t1$parameter |> as.character())
    expect_equal(ttest$results$values[3], t1$p.value |> as.character())
    expect_equal(ttest$results$values[4], t1$conf.int[1] |> as.character())
    expect_equal(ttest$results$values[5], t1$conf.int[2] |> as.character())
    expect_equal(ttest$results$values[6], t1$estimate |> as.character())
    expect_equal(ttest$results$values[7], t1$null.value |> as.character())
    expect_equal(ttest$results$values[8], t1$stderr |> as.character())
    expect_equal(ttest$results$values[9], t1$alternative |> as.character())
    expect_equal(ttest$results$values[10], t1$method |> as.character())
    expect_equal(ttest$results$values[11], 'Petal.Length')
  })
})

# test one t test - histogram -------------------------------------------------
test_that('One-sample t Test module - histogram', {
  testServer(one_t_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$conf <- reactiveValues(
      plot_fill_color = '#229999',
      plot_line_color = '#44aa44',
      plot_limit = 1e5
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

    # wait for the extended_task histogram
    while (task_hist$status() == 'running') {
      session$flushReact()
    }

    expect_equal(task_hist$status(), 'success')
    expect_s3_class(ttest_hist(), c('gg', 'ggplot'))
  })
})

