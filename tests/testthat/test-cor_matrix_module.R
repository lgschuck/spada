# tests/testthat/test-cor_matrix_module.R

# test correlation matrix -----------------------------------------------------
test_that('test correlation matrix - pearson method', {
  testServer(cor_matrix_server, {
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

    # inputs ----------------------------------------------------------------
    session$setInputs(
      sel_vars = c('Sepal.Length', 'Sepal.Width'),
      radio_method = 'pearson',
      btn_cor_matrix = 1
    )

    expect_s3_class(df_cor(), 'data.table')

    expect_equal(c('Var1', 'Var2', 'value', 'label', 'method'), names(df_cor()))
    expect_equal(nrow(df_cor()), 4)

    expect_equal(
      df_cor()[Var1 == 'Sepal.Length' & Var2 == 'Sepal.Width', round(value, 3)],
      round(cor(iris$Sepal.Length, iris$Sepal.Width), 3)
    )
  })
})
