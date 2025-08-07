# tests/testthat/test-normality_test_module.R

df_test <- data.frame(
  var1 = rnorm(100)
)

# test normality test module - variable selection -------------------------------------
test_that('Normality Test module - variable selection', {
  testServer(normality_test_server, {
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

    session$setInputs(sel_var = 'Setal.Width')

    expect_equal(var_analysis(), setdiff(names(iris), 'Species'))
    expect_equal(var(), iris$Setal.Width)
    expect_equal(var_len(), iris$Setal.Width |> length())

  })
})

# test normality test - test plots --------------------------------------------
test_that('Z Test module - test plots', {
  testServer(normality_test_server, {
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
      plot_line_color = '#44aa44'
    )

    session$setInputs(
      sel_var = 'Petal.Length',
      bins = 5,
      btn_qq = 1,
      btn_hist = 1
    )

    expect_s3_class(norm_hist(), c('gg', 'ggplot'))
    expect_s3_class(norm_qq_plot(), c('gg', 'ggplot'))
  })
})

# test normality test - ks test -------------------------------------------------------
test_that('Normality Test module - ks test results', {
  testServer(normality_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('df' = df_test |> as.data.table()),
      act_name = 'df'
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
      plot_line_color = '#44aa44'
    )

    session$setInputs(
      sel_var = 'var1',
      btn_ks = 1
    )

    ks1 <- ks.test(df_test$var1, 'pnorm')

    expect_s3_class(ks_results_gt(), 'gt_tbl')
    expect_equal(var(), df_test$var1)
    expect_equal(ks_results()$results |> nrow(), 6)
    expect_equal(ks_results()$results |> ncol(), 2)
    expect_equal(ks_results()$results |> names(), c('values', 'results'))

    expect_equal(ks_results()$results$values[1], ks1$statistic |> as.character())
    expect_equal(ks_results()$results$values[2], ks1$p.value |> as.character())
    expect_equal(ks_results()$results$values[3], ks1$alternative |> as.character())
    expect_equal(ks_results()$results$values[4], ks1$method |> as.character())
    expect_equal(ks_results()$results$values[5], 'var1')
    expect_equal(ks_results()$results$values[6], ks1$exact |> as.character())
  })
})

# test normality test - shapiro wilk test -------------------------------------
test_that('Normality Test module - shapiro wilk test results', {
  testServer(normality_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('df' = df_test |> as.data.table()),
      act_name = 'df'
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
      plot_line_color = '#44aa44'
    )

    session$setInputs(
      sel_var = 'var1',
      btn_sw = 1
    )

    sw1 <- shapiro.test(df_test$var1)

    expect_s3_class(sw_results_gt(), 'gt_tbl')
    expect_equal(var(), df_test$var1)
    expect_equal(sw_results() |> nrow(), 4)
    expect_equal(sw_results() |> ncol(), 2)
    expect_equal(sw_results() |> names(), c('values', 'results'))

    expect_equal(sw_results()$values[1], sw1$statistic |> as.character())
    expect_equal(sw_results()$values[2], sw1$p.value |> as.character())
    expect_equal(sw_results()$values[3], sw1$method |> as.character())
    expect_equal(sw_results()$values[4], 'var1')
  })
})

# test normality test - shapiro francia test -------------------------------------
test_that('Normality Test module - shapiro francia test results', {
  testServer(normality_test_server, {
    session$userData$dt <- reactiveValues(
      dt = list('df' = df_test |> as.data.table()),
      act_name = 'df'
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
      plot_line_color = '#44aa44'
    )

    session$setInputs(
      sel_var = 'var1',
      btn_sf = 1
    )

    sf1 <- ShapiroFranciaTest(df_test$var1)

    expect_s3_class(sf_results_gt(), 'gt_tbl')
    expect_equal(var(), df_test$var1)
    expect_equal(sf_results() |> nrow(), 4)
    expect_equal(sf_results() |> ncol(), 2)
    expect_equal(sf_results() |> names(), c('values', 'results'))

    expect_equal(sf_results()$values[1], sf1$statistic |> as.character())
    expect_equal(sf_results()$values[2], sf1$p.value |> as.character())
    expect_equal(sf_results()$values[3], sf1$method |> as.character())
    expect_equal(sf_results()$values[4], 'var1')
  })
})
