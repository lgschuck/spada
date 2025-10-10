# tests/testthat/test-correlation_module.R

# test correlation - pearson --------------------------------------------------
test_that('test correlation - pearson method', {
  testServer(correlation_server, {
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

    r1 <- cor.test(
      iris$Petal.Length,
      iris$Sepal.Length,
      alternative = 'two.sided',
      method = 'p',
      conf.level = 0.95,
      exact = F
    )

    session$setInputs(
      sel_var1 = 'Petal.Length',
      sel_var2 = 'Sepal.Length',
      radio_method = 'pearson',
      radio_alternative = 'two.sided',
      confidence = 95,
      btn_run_test = 1
    )

    expect_s3_class(cor_results_gt(), 'gt_tbl')
    expect_equal(cor_test$results |> nrow(), 10)
    expect_equal(cor_test$results |> ncol(), 2)
    expect_equal(cor_test$results |> names(), c('values', 'results'))
    expect_equal(cor_test$results$values[1], r1$statistic |> as.character())
    expect_equal(cor_test$results$values[2], r1$parameter |> as.character())
    expect_equal(cor_test$results$values[3], r1$p.value |> as.character())
    expect_equal(cor_test$results$values[4], r1$estimate |> as.character())
    expect_equal(cor_test$results$values[5], r1$null.value |> as.character())
    expect_equal(cor_test$results$values[6], r1$alternative |> as.character())
    expect_equal(cor_test$results$values[7], r1$method |> as.character())
    expect_equal('Petal.Length / Sepal.Length', cor_test$results$values[8])

  })
})

# test correlation - kendall --------------------------------------------------
test_that('test correlation - kendall method', {
  testServer(correlation_server, {
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

    r1 <- cor.test(
      iris$Petal.Length,
      iris$Sepal.Length,
      alternative = 'two.sided',
      method = 'kendall',
      conf.level = 0.95,
      exact = F
    )

    session$setInputs(
      sel_var1 = 'Petal.Length',
      sel_var2 = 'Sepal.Length',
      radio_method = 'kendall',
      radio_alternative = 'two.sided',
      confidence = 95,
      btn_run_test = 1
    )

    expect_s3_class(cor_results_gt(), 'gt_tbl')
    expect_equal(cor_test$results |> nrow(), 7)
    expect_equal(cor_test$results |> ncol(), 2)
    expect_equal(cor_test$results |> names(), c('values', 'results'))
    expect_equal(cor_test$results$values[1], r1$statistic |> as.character())
    expect_equal(cor_test$results$values[2], r1$p.value |> as.character())
    expect_equal(cor_test$results$values[3], r1$estimate |> as.character())
    expect_equal(cor_test$results$values[4], r1$null.value |> as.character())
    expect_equal(cor_test$results$values[5], r1$alternative |> as.character())
    expect_equal(cor_test$results$values[6], r1$method |> as.character())
    expect_equal('Petal.Length / Sepal.Length', cor_test$results$values[7])

  })
})

# test correlation - spearman -------------------------------------------------
test_that('test correlation - spearman method', {
  testServer(correlation_server, {
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

    r1 <- cor.test(
      iris$Petal.Length,
      iris$Sepal.Length,
      alternative = 'two.sided',
      method = 'spearman',
      conf.level = 0.95,
      exact = F
    )

    session$setInputs(
      sel_var1 = 'Petal.Length',
      sel_var2 = 'Sepal.Length',
      radio_method = 'spearman',
      radio_alternative = 'two.sided',
      confidence = 95,
      btn_run_test = 1
    )

    expect_s3_class(cor_results_gt(), 'gt_tbl')
    expect_equal(cor_test$results |> nrow(), 7)
    expect_equal(cor_test$results |> ncol(), 2)
    expect_equal(cor_test$results |> names(), c('values', 'results'))
    expect_equal(cor_test$results$values[1], r1$statistic |> as.character())
    expect_equal(cor_test$results$values[2], r1$p.value |> as.character())
    expect_equal(cor_test$results$values[3], r1$estimate |> as.character())
    expect_equal(cor_test$results$values[4], r1$null.value |> as.character())
    expect_equal(cor_test$results$values[5], r1$alternative |> as.character())
    expect_equal(cor_test$results$values[6], r1$method |> as.character())
    expect_equal('Petal.Length / Sepal.Length', cor_test$results$values[7])

  })
})

# test correlation - scatter plot -------------------------------------------------
test_that('test correlation - scatter plot', {
  testServer(correlation_server, {
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

    session$userData$conf <- reactiveValues(
      plot_fill_color = '#229999',
      plot_line_color = '#44aa44',
      plot_limit = 1e5
    )

    session$setInputs(
      sel_var1 = 'Petal.Length',
      sel_var2 = 'Sepal.Length',
      btn_scatter = 1
    )

    expect_s3_class(scatter_plot(), c('gg', 'ggplot'))

  })
})
