# tests/testthat/test-rename_cols_module.R

# test rename one column ------------------------------------------------------
test_that("Test rename one column", {
  testServer(rename_cols_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris),
      act_name = 'iris'
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
      dt = list('mtcars' = mtcars[c('hp', 'mpg')]),
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
      dt = list('mtcars' = mtcars[c('hp', 'mpg', 'cyl')]),
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
      dt = list('iris' = iris),
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
      dt = list('iris' = iris),
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
      dt = list('iris' = iris),
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

