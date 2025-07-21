# tests/testthat/test-convert_cols_module.R

# test textInput - type/class -------------------------------------------------
test_that("Test currernt type/class ", {
  testServer(convert_cols_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$setInputs(vars_sel = 'Sepal.Length')

    expect_equal(
      paste('Type: [', iris$Sepal.Length |> typeof(), '] |',
            'Class: [',
            paste(iris$Sepal.Length |> class(), collapse = '/'),
            ']'),
      current_format())
  })
})

# test sample trigger reactiveVal ---------------------------------------------
test_that("Test Sample trigger - updates on button click", {
  testServer(convert_cols_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    trigger_before <- preview_sample_trigger()

    session$setInputs(btn_preview_sample = 1)
    trigger_after <- preview_sample_trigger()

    expect_true(trigger_after > trigger_before)
  })
})

# test preview df -------------------------------------------------------------
test_that("Test Preview dataframe after variable and format selection", {
  testServer(convert_cols_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$setInputs(vars_sel = 'Species')
    session$setInputs(sel_format = 'as.character')

    expect_true(is.data.frame(preview_df()))
    expect_true(ncol(preview_df()) == 2)
    expect_true(nrow(preview_df()) == 8)
    expect_equal(preview_df(),
                 data.frame(Species = iris[preview_sample(), 5],
                            preview = iris[preview_sample(), 5] |>
                              as.character())
                 |> as.data.table()
    )
  })
})

# test conversion -------------------------------------------------------------
test_that("Test Conversion applies when button clicked", {
  testServer(convert_cols_server, {
    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )

    session$setInputs(vars_sel = 'Species')
    session$setInputs(sel_format = 'as.character')

    original_class <- class(get_act_dt(session)[['Species']])

    session$setInputs(btn_apply = 1)

    converted_class <- class(get_act_dt(session)[['Species']])

    expect_false(identical(original_class, converted_class))
    expect_equal(converted_class, 'character')
    expect_equal(get_act_dt(session)[['Species']] |> class(), 'character')
    expect_equal(get_act_dt(session) |> class(), c('data.table', 'data.frame'))

  })
})

