# tests/testthat/test-exploratory_module.R

df_test <- data.frame(
  cyl = as.factor(mtcars$cyl),
  am = as.factor(mtcars$am)
)

# test exploratory test module - variable selection ---------------------------
test_that('Exploratory module - variable selection', {
  testServer(exploratory_server, {
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
      sel_vars = 'Petal.Length',
      sel_vars2 = 'Petal.Width',
      outliers = FALSE
    )

    expect_equal(var_analysis(), names(iris))
    expect_equal(var(), iris$Petal.Length)
    expect_equal(var2(), iris$Petal.Width)

  })
})

# test exploratory test module - dist plot ------------------------------------
test_that('Exploratory module - dist plot', {
  testServer(exploratory_server, {
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
      sel_vars = 'Petal.Length',
      sel_vars2 = 'Petal.Width',
      outliers = FALSE,
      radio_dist_plot = 'hist',
      var_percentile = 90,
      bins = 10
    )

    expect_equal(var_analysis(), names(iris))
    expect_equal(var(), iris$Petal.Length)
    expect_equal(var2(), iris$Petal.Width)
    expect_s3_class(dist_plot(), c('gg', 'ggplot'))

  })
})

# test exploratory test module - scatter plot ---------------------------------
test_that('Exploratory module - scatter plot', {
  testServer(exploratory_server, {
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
      sel_vars = 'Petal.Length',
      sel_vars2 = 'Petal.Width',
      outliers = FALSE,
      radio_dist_plot = 'hist',
      var_percentile = 90,
      bins = 10,
      btn_scatter = 1,
      scatter_lm = FALSE
    )

    expect_equal(var_analysis(), names(iris))
    expect_equal(var(), iris$Petal.Length)
    expect_equal(var2(), iris$Petal.Width)
    expect_s3_class(scatter_plot(), c('gg', 'ggplot'))

  })
})

# test exploratory test module - tables ---------------------------------------
test_that('Exploratory module - tables - 1 variable', {
  testServer(exploratory_server, {
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
      sel_vars = 'Species',
      sel_vars2 = 'Petal.Width',
      outliers = FALSE,
      table_var = '1v',
      table_type = 'abs_table'
    )

    expect_equal(var(), iris$Species)
    expect_equal(table_values(), table(iris$Species) |> as.data.frame())
    expect_s3_class(table_values(), 'data.frame')
    expect_s3_class(table_values_gt(), 'gt_tbl')
  })
})

test_that('Exploratory module - tables - 2 variables', {
  testServer(exploratory_server, {
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
      sel_vars = 'cyl',
      sel_vars2 = 'am',
      outliers = FALSE,
      table_var = '2v',
      table_type = 'abs_table'
    )

    tab1 <- table(df_test$cyl, df_test$am)
    tab1 <- tab1 |> as.data.frame.matrix()
    tab1 <- cbind(var1 = rownames(tab1), tab1)

    expect_equal(var(), df_test$cyl)
    expect_equal(table_values(), tab1)
    expect_s3_class(table_values(), 'data.frame')
    expect_s3_class(table_values_gt(), 'gt_tbl')
  })
})

# test exploratory test module - linear model ---------------------------------
test_that('Exploratory module - linear model', {
  testServer(exploratory_server, {
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
      sel_vars = 'Petal.Width',
      sel_vars2 = 'Petal.Length',
      outliers = FALSE,
      btn_scatter_lm_run = 1
    )

    m1 <- lm(Petal.Width ~ Petal.Length, data = iris, model = F)

    expect_true(!is.null(linear_model$model))
    expect_equal(linear_model$y_name, 'Petal.Width')
    expect_equal(linear_model$x_name, 'Petal.Length')
    expect_equal(linear_model$y, m1$fitted.values)
    expect_equal(summary(linear_model$model)$r.squared,
                 summary(m1)$r.squared)

    expect_equal(summary(linear_model$model)$r.squared,
                 summary(m1)$r.squared)

    expect_equal(summary(linear_model$model)$residuals,
                 summary(m1)$residuals)

    expect_equal(summary(linear_model$model)$fstatistic,
                 summary(m1)$fstatistic)

    expect_s3_class(lm_var_table(), 'gt_tbl')
    expect_s3_class(lm_metrics(), 'gt_tbl')


    session$setInputs(radio_lm_resid = 'hist')

    expect_equal(update_lm_resid_plot(), 0)

    session$setInputs(btn_lm_resid = 1)

    expect_equal(update_lm_resid_plot(), 1)

    expect_s3_class(lm_resid_plot(), 'ggplot')

    # clear model
    session$setInputs(btn_scatter_lm_clear = 1)
    expect_null(linear_model$model)
    expect_equal(update_lm_resid_plot(), 2)

    expect_null(linear_model$model$residuals)
    expect_null(linear_model$x)
    expect_null(linear_model$y)
    expect_equal(linear_model$x_name, '')
    expect_equal(linear_model$y_name, '')
  })
})

# test exploratory test module - stats ----------------------------------------
test_that('Exploratory module - stats', {
  testServer(exploratory_server, {
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
      sel_vars = 'Petal.Width',
      sel_vars2 = 'Petal.Length',
      outliers = FALSE
    )

    expect_equal(stats_sd(), sd(iris$Petal.Width))
    expect_equal(stats_correlation(), cor(iris$Petal.Width, iris$Petal.Length))

  })
})

