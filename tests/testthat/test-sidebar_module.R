# tests/testthat/test-sidebar_module.R

# test df info UI -------------------------------------------------------------
test_that("Test render df_info UI", {
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

    expect_true(is.list(output$df_info))

    expect_true(grepl('df_test', output$df_info$html))
    expect_true(grepl('Size', output$df_info$html))
    expect_true(grepl('0', output$df_info$html))
    expect_true(grepl('Rows/Columns:\n  5.0 / 2\n', output$df_info$html))
    expect_true(grepl("Columns with NA's:\n  1", output$df_info$html))

  })
})

# test render preview ---------------------------------------------------------
test_that("Test renders dataset preview first dataset", {
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

test_that("Test renders dataset preview second dataset", {
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

