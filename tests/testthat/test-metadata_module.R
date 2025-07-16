# tests/testthat/test-metadata_module.R

# test gt ---------------------------------------------------------------------
test_that("Test gt object", {
  testServer(metadata_server, {

    # create elements from outside of module
    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = as.data.table(mtcars), 'iris' = as.data.table(iris)),
      act_name = 'mtcars'
    )

    session$userData$dt_names <- reactive({
      names(session$userData$dt$dt)
    })

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$gt_info <- reactive({
      req(session$userData$dt$df_info())

      Map(gt_info, session$userData$dt$df_info(),
          df_name = names(session$userData$dt$df_info()))
    })

    # start test -----------------------------
    session$setInputs(dataset_sel = 'mtcars')

    expect_equal(names(session$userData$dt$gt_info()), c('mtcars', 'iris'))
    expect_equal(class(session$userData$dt$gt_info()), 'list')
    expect_equal(class(session$userData$dt$gt_info()[[1]]), c('gt_tbl', 'list'))
    expect_equal(class(session$userData$dt$gt_info()[[2]]), c('gt_tbl', 'list'))

    expect_equal(session$userData$dt$gt_info()[[1]][[1]][['var']],
                 names(mtcars))

    expect_equal(session$userData$dt$gt_info()[[2]][[1]],
                 gt_info(df_info(iris), df_name = 'mtcars')[[1]]
    )

    expect_equal(session$userData$dt$gt_info()[[2]][[1]][['var']],
                 names(iris))

    expect_equal(session$userData$dt$gt_info()[[2]][[1]],
                 gt_info(df_info(iris), df_name = 'iris')[[1]]
    )

  })
})

