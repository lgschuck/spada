# tests/testthat/test-sidebar_module.R

# test change active dataset --------------------------------------------------
test_that('Test change active dataset', {
  testServer(sidebar_server, args = list(app_session = NULL), {

    session$userData$dt <- reactiveValues(
      dt = list('df_iris' = iris |> as.data.table(),
                'df_mtcars' = mtcars |> as.data.table()),
      act_name = 'df_iris'
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$setInputs(sel_datasets_names = 'iris')

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$setInputs(sel_act_dt = 'df_mtcars')

    expect_true(session$userData$dt$act_name == 'df_mtcars')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table())

    session$setInputs(sel_act_dt = 'df_iris')

    expect_true(session$userData$dt$act_name == 'df_iris')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table())

    session$setInputs(sel_act_dt = 'df_mtcars')

    expect_true(session$userData$dt$act_name == 'df_mtcars')
    expect_equal(session$userData$dt$dt |> names(), c('df_iris', 'df_mtcars'))
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table()
    )
  })
})

test_that('Test active dataset - after deletion', {
  testServer(sidebar_server, args = list(app_session = NULL), {


    session$userData$dt <- reactiveValues(
      dt = list('df_iris' = iris |> as.data.table(),
                'df_mtcars' = mtcars |> as.data.table(),
                'df_cars' = cars |> as.data.table()
                ),
      act_name = 'df_iris'
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$setInputs(sel_datasets_names = 'iris')

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$setInputs(sel_act_dt = 'df_mtcars')

    expect_true(session$userData$dt$act_name == 'df_mtcars')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table())

    session$setInputs(sel_act_dt = 'df_iris')

    expect_true(session$userData$dt$act_name == 'df_iris')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 iris |> as.data.table())

    session$setInputs(sel_act_dt = 'df_mtcars')

    expect_true(session$userData$dt$act_name == 'df_mtcars')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table()
    )

    # delete dataset
    session$userData$dt$dt[['df_cars']] <- NULL

    expect_true(session$userData$dt$act_name == 'df_mtcars')
    expect_equal(session$userData$dt$dt |> names(), c('df_iris', 'df_mtcars'))
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 mtcars |> as.data.table())
  })
})

# test df info UI -------------------------------------------------------------
test_that('Test render df_info UI', {
  testServer(sidebar_server, args = list(app_session = NULL), {

    session$userData$dt <- reactiveValues(
      dt = list('df_test' = data.frame(var1 = 1:5, var2 = rep(NA, 5) |>
          as.data.table())),
      act_name = 'df_test'
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$df_info()[[session$userData$dt$act_name]]
    })

    session$userData$dt$act_mini_meta <- reactive({
      req(session$userData$dt$act_meta())

      list(
        'row_col' = paste(session$userData$dt$act_meta() |>
                            pull(rows) |> head(1) |> f_num(dig = 1),
                          '/',
                          session$userData$dt$act_meta() |>
                            pull(cols) |> head(1) |> f_num()),
        'col_nas' = session$userData$dt$act_meta() |>
          filter(n_nas > 0) |> nrow(),
        'size' = (object.size(get_act_dt(session)) / 2^20) |>
          as.numeric() |> round(2)
      )
    })

    expect_true(is.list(output$df_info))
    expect_true(grepl('Size', output$df_info$html))
    expect_true(grepl('Rows/Columns:', output$df_info$html))
    expect_true(grepl("Columns with NA's:", output$df_info$html))

    expect_equal(output$row_col, '5.0 / 2')
    expect_equal(output$col_nas, '1')
    expect_equal(output$size_mb, '0')
  })
})

# test render preview ---------------------------------------------------------
test_that('Test renders dataset preview first dataset', {
  testServer(sidebar_server, args = list(app_session = NULL), {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$setInputs(sel_datasets_names = 'iris')

    expect_true(is.list(output$df_preview))
    expect_true(names(output$df_preview)[1] == 'html')
    expect_true(grepl('iris', output$df_preview$html))
    expect_true(grepl('Sepal.Length', output$df_preview$html))
    expect_true(grepl('Sepal.Width', output$df_preview$html))
    expect_true(grepl('Petal.Length', output$df_preview$html))
    expect_true(grepl('Petal.Width', output$df_preview$html))
    expect_true(grepl('Species', output$df_preview$html))

  })
})

test_that('Test renders dataset preview second dataset', {
  testServer(sidebar_server, args = list(app_session = NULL), {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table(),
                'cars' = cars |> as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$setInputs(sel_datasets_names = 'cars')

    expect_true(is.list(output$df_preview))
    expect_true(names(output$df_preview)[1] == 'html')
    expect_true(grepl('cars', output$df_preview$html))
    expect_true(grepl('speed', output$df_preview$html))
    expect_true(grepl('dist', output$df_preview$html))

  })
})

