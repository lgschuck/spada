# tests/testthat/test-data_overview_module.R

# test validate number of rows ------------------------------------------------
test_that("Test sample rows", {
  testServer(data_overview_server, {

    session$userData$dt <- reactiveValues(dt = list('iris' = as.data.table(iris),
                                                    'mtcars' = as.data.table(mtcars)))

    session$userData$df <- reactiveValues(act = iris |> as.data.table())
    session$userData$df$act_name = 'iris'
    session$userData$dt_names <- reactive({
      names(session$userData$dt$dt)
    })

    session$setInputs(
      dataset_sel = 'iris',
      size_sample = 0)

    expect_error(idx(), "Number of rows must be > 0")
  })
})

# test first rows -------------------------------------------------------------
test_that("Test first rows", {
  testServer(data_overview_server, {

    session$userData$dt <- reactiveValues(dt = list('iris' = as.data.table(iris),
                                                    'mtcars' = as.data.table(mtcars)))
    session$userData$df <- reactiveValues(act = iris |> as.data.table())
    session$userData$df$act_name = 'iris'
    session$userData$dt_names <- reactive({
      names(session$userData$dt$dt)
    })

    session$setInputs(size_sample = 5,
                      dataset_sel = 'iris',
                      radio_sample = 'first')

    expect_equal(data_filtered(), iris[1:5,])

  })
})

# test last rows -------------------------------------------------------------
test_that("Test last rows", {
  testServer(data_overview_server, {

    session$userData$dt <- reactiveValues(dt = list('iris' = as.data.table(iris),
                                                    'mtcars' = as.data.table(mtcars)))
    session$userData$df <- reactiveValues(act = mtcars |> as.data.table())
    session$userData$df$act_name = 'mtcars'
    session$userData$dt_names <- reactive({
      names(session$userData$dt$dt)
    })

    session$setInputs(size_sample = 3,
                      dataset_sel = 'mtcars',
                      radio_sample = 'last')

    expect_equal(data_filtered(), mtcars[30:32, ], ignore_attr = TRUE)

  })
})

# test sample of rows ---------------------------------------------------------
test_that("Test sample rows", {
  testServer(data_overview_server, {

    session$userData$dt <- reactiveValues(dt = list('iris' = as.data.table(iris),
                                                    'mtcars' = as.data.table(mtcars)))
    session$userData$df <- reactiveValues(act = iris |> as.data.table())
    session$userData$df$act_name = 'iris'
    session$userData$dt_names <- reactive({
      names(session$userData$dt$dt)
    })

    session$setInputs(size_sample = 5,
                      dataset_sel = 'iris',
                      radio_sample = 'first')

    # test for equality (unlist to avoid error caused by row.names)
    expect_equal(data_filtered() |> unlist(), iris[idx(),] |> unlist())
  })
})
