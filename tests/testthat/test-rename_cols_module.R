# tests/testthat/test-rename_cols_module.R

# test rename one column ------------------------------------------------------
test_that("Test rename one column", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris',
      updated_cols = NULL,
      data_changed_rename = FALSE,
      data_changed_reorder = FALSE
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(vars_sel = 'Species')
    session$setInputs(txt_new_name = 'Species_new_name')
    session$setInputs(btn_rename = 1)

    expect_equal(names(get_act_dt(session))[5], 'Species_new_name')
  })
})

# test rename prefix ----------------------------------------------------------
test_that("Test rename prefix - two columns", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars[c('hp', 'mpg')] |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(
      vars_sel_multi = c('hp', 'mpg'),
      rename_method = 'prefix_suffix',
      part_position = 'prefix',
      name_separator = '_',
      txt_new_name_multi = 'new')
    session$setInputs(btn_rename_multi = 1)

    expect_equal(names(get_act_dt(session)), c('new_hp', 'new_mpg'))
  })
})

# test rename suffix ----------------------------------------------------------
test_that("Test rename suffix - three columns", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = mtcars[c('hp', 'mpg', 'cyl')] |> as.data.table()),
      act_name = 'mtcars'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(
      vars_sel_multi = c('hp', 'mpg', 'cyl'),
      rename_method = 'prefix_suffix',
      part_position = 'suffix',
      name_separator = '..',
      txt_new_name_multi = 'new_suffix')
    session$setInputs(btn_rename_multi = 1)

    expect_equal(names(get_act_dt(session)),
                 c('hp..new_suffix', 'mpg..new_suffix', 'cyl..new_suffix'))
  })
})

# test rename function ---------------------------------------------------------
test_that("Test rename function - one column", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(
      vars_sel_multi = c('Species'),
      rename_method = 'function',
      name_function = 'toupper'
    )
    session$setInputs(btn_rename_multi = 1)

    expect_equal(names(get_act_dt(session))[5], 'SPECIES')
  })
})

# test rename replace ---------------------------------------------------------
test_that("Test rename replace - four columns", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(
      vars_sel_multi = c('Sepal.Length', 'Sepal.Width', 'Petal.Length',
                         'Petal.Width'),
      rename_method = 'replace',
      txt_replace_part = '.',
      txt_replace_new_part = '_'
      )
    session$setInputs(btn_rename_multi = 1)

    expect_equal(names(get_act_dt(session)),
                 c('Sepal_Length', 'Sepal_Width', 'Petal_Length', 'Petal_Width',
                   'Species'))
  })
})

# test rename remove ----------------------------------------------------------
test_that("Test rename remove - two columns", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)
    session$setInputs(
      vars_sel_multi = c('Sepal.Length', 'Sepal.Width'),
      rename_method = 'remove',
      txt_remove_part = '.'
    )
    session$setInputs(btn_rename_multi = 1)

    expect_equal(names(get_act_dt(session))[1:2], c('SepalLength', 'SepalWidth'))
  })
})

# test input check messages ---------------------------------------------------
test_that('Test rename - check inputs', {
  testServer(rename_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text },
      remove_running_modal = function(){last_msg <<- 'Remove modal'}
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(vars_sel = NULL, btn_rename = 1)

    expect_equal(last_msg, 'Select at least one variable')

    session$setInputs(vars_sel = 'Species', txt_new_name = 123, btn_rename = 2)

    expect_equal(last_msg, 'New name is not valid or already in use')

    session$setInputs(vars_sel = 'Species', txt_new_name = 'v123', btn_rename = 3)

    expect_equal(last_msg, 'Remove modal')
  })
})


test_that('Test rename multi - check inputs', {
  testServer(rename_cols_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |> as.data.table()),
      act_name = 'iris'
    )
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(vars_sel_multi = NULL, btn_rename_multi = 1)

    expect_equal(last_msg, 'Select at least one variable')

    session$setInputs(vars_sel_multi = 'Species',
                      rename_method = 'prefix_suffix',
                      txt_new_name_multi = NULL,
                      btn_rename_multi = 2
    )

    expect_equal(last_msg, 'Inform a new part to insert')

    session$setInputs(vars_sel_multi = 'Species',
                      rename_method = 'replace',
                      txt_replace_part = NULL,
                      btn_rename_multi = 3
    )

    expect_equal(last_msg, 'Inform the text to replace')

    session$setInputs(vars_sel_multi = 'Species',
                      rename_method = 'remove',
                      txt_remove_part = NULL,
                      btn_rename_multi = 4
    )

    expect_equal(last_msg, 'Inform the text to be removed')

    session$setInputs(vars_sel_multi = 'Species',
                      rename_method = 'replace',
                      txt_replace_part = 'Sp',
                      txt_replace_new_part = 'Sp',
                      btn_rename_multi = 5
    )

    expect_equal(last_msg, 'No changes to apply')

    session$setInputs(vars_sel_multi = 'Petal.Length',
                      txt_replace_part = 'Petal',
                      txt_replace_new_part = 'Sepal',
                      btn_rename_multi = 6
    )

    expect_equal(last_msg, 'Some new names already in use')

    session$setInputs(vars_sel_multi = 'Petal.Length',
                      txt_replace_part = 'Petal.Length',
                      txt_replace_new_part = 123,
                      btn_rename_multi = 7
    )

    expect_equal(last_msg, 'New names are not valid')
  })
})

