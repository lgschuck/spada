# tests/testthat/test-exploratory_module.R

# test exploratory test module - variable selection -------------------------------------
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

# test exploratory test module - scatter plot ------------------------------------
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


