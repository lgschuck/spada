# tests/testthat/test-lm_module.R

# test lm module - variable selection -----------------------------------------
test_that('lm_server test variable selection', {
  testServer(lm_server, {

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
      sel_yvar = 'Petal.Length',
      sel_xvar = c('Sepal.Length')
    )

    expect_equal(var_analysis(), names(iris))
    expect_equal(yvar(), setdiff(names(iris), 'Species'))
    expect_equal(xvar(), setdiff(names(iris), c('Petal.Length')))
  })
})

# test lm module - run model --------------------------------------------------
test_that('lm_server - test run model', {
  testServer(lm_server, {

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
      sel_yvar = 'Petal.Length',
      sel_xvar = c('Sepal.Length'),
      btn_run_lm = 1
    )

    expect_equal(linear_model$y_name, 'Petal.Length')
    expect_equal(linear_model$x_name, 'Sepal.Length')

    m1 <- lm(Petal.Length ~ Sepal.Length, data = iris, model = F)
    s1 <- summary(m1)

    expect_equal(linear_model$summary$coefficients, s1$coefficients)
    expect_equal(linear_model$summary$r.squared, s1$r.squared)
    expect_equal(linear_model$summary$adj.r.squared, s1$adj.r.squared)
    expect_equal(linear_model$summary$fstatistic, s1$fstatistic)
  })
})

# test lm module - gt tables --------------------------------------------------
test_that('lm_server - test gt tables', {
  testServer(lm_server, {

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
      sel_yvar = 'Petal.Length',
      sel_xvar = c('Sepal.Length'),
      btn_run_lm = 1
    )

    expect_s3_class(lm_var_table(), 'gt_tbl')
    expect_s3_class(lm_metrics(), 'gt_tbl')
  })
})


# test lm module - save model -------------------------------------------------
test_that('lm_server - save model', {
  testServer(lm_server, {
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
      sel_yvar = 'Petal.Length',
      sel_xvar = c('Sepal.Length'),
      btn_run_lm = 1,
      btn_save_model = 1,
      model_filename = 'my_model'
    )

    expect_equal(output$down_handler |> basename(), 'my_model.RDS')
  })
})

