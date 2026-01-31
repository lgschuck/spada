# tests/testthat/test-groupby_module.R

dt <- mtcars |> as.data.table()

# test group by - no groupby var ----------------------------------------------
test_that('Test group by - no group by var - overwrite', {
  testServer(groupby_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = dt),
      act_name = 'mtcars',
      meta = list('mtcars' = mtcars |> df_info())
    )

    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$act_meta <- reactive(dt |> df_info())
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_groupby = NULL,
      vars_sel = 'mpg',
      fun = 'min',
      txt_new_name = 'min_mpg',
      radio_overwrite = 'overwrite'
    )

    session$setInputs(btn_add_var = 1)

    session$setInputs(btn_groupby = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(min_mpg = min(mpg)), by = NULL |> as.list()]
    )

    session$setInputs(vars_groupby = c())

    session$setInputs(btn_groupby = 2)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(min_mpg = min(mpg)), by = c() |> as.list()]
    )
  })
})

# test group by - add and group -----------------------------------------------
test_that('Test group by - 1x1 var - overwrite', {
  testServer(groupby_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = dt),
      act_name = 'mtcars',
      meta = list('mtcars' = mtcars |> df_info())
    )

    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$act_meta <- reactive(dt |> df_info())
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_groupby = 'cyl',
      vars_sel = 'mpg',
      fun = 'min',
      txt_new_name = 'min_mpg',
      radio_overwrite = 'overwrite'
    )

    session$setInputs(btn_add_var = 1)

    session$setInputs(btn_groupby = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(min_mpg = min(mpg)), by = 'cyl']
    )
  })
})

test_that('Test group by - 2x1 var - overwrite', {
  testServer(groupby_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = dt),
      act_name = 'mtcars',
      meta = list('mtcars' = mtcars |> df_info())
    )

    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$act_meta <- reactive(dt |> df_info())
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_groupby = c('cyl', 'am'),
      vars_sel = 'hp',
      fun = 'max',
      txt_new_name = 'max_hp',
      radio_overwrite = 'overwrite'
    )
    session$setInputs(btn_add_var = 1)

    session$setInputs(btn_groupby = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(max_hp = max(hp)), by = c('cyl', 'am')]
    )
  })
})

test_that('Test group by - 2x2 var - overwrite', {
  testServer(groupby_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = dt),
      act_name = 'mtcars',
      meta = list('mtcars' = mtcars |> df_info())
    )

    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$act_meta <- reactive(dt |> df_info())
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_groupby = c('cyl', 'am'),
      vars_sel = 'hp',
      fun = 'sum',
      txt_new_name = 'sum_hp',
      radio_overwrite = 'overwrite'
    )
    session$setInputs(btn_add_var = 1)

    session$setInputs(
      vars_sel = 'mpg',
      fun = 'mean',
      txt_new_name = 'mean_mpg',
      radio_overwrite = 'overwrite'
    )
    session$setInputs(btn_add_var = 2)

    session$setInputs(btn_groupby = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(sum_hp = sum(hp),
             mean_mpg = mean(mpg)), by = c('cyl', 'am')]
    )
  })
})

test_that('Test group by - 2x2 var - new dataset', {
  testServer(groupby_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = dt),
      act_name = 'mtcars',
      meta = list('mtcars' = mtcars |> df_info())
    )

    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$act_meta <- reactive(dt |> df_info())
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_groupby = c('cyl', 'am'),
      vars_sel = 'hp',
      fun = 'sum',
      txt_new_name = 'sum_hp',
      radio_overwrite = 'new',
      txt_new_dt_name = 'new_dt'
    )
    session$setInputs(btn_add_var = 1)

    session$setInputs(
      vars_sel = 'mpg',
      fun = 'mean',
      txt_new_name = 'mean_mpg'
    )
    session$setInputs(btn_add_var = 2)

    session$setInputs(btn_groupby = 1)

    expect_equal(
      session$userData$dt$dt[['new_dt']],
      dt[, .(sum_hp = sum(hp),
             mean_mpg = mean(mpg)), by = c('cyl', 'am')]
    )

    expect_equal(
      session$userData$dt$meta[['new_dt']],
      dt[, .(sum_hp = sum(hp),
             mean_mpg = mean(mpg)), by = c('cyl', 'am')] |> df_info()
    )
  })
})

# test group by - overwrite - metadata update ---------------------------------
# test using spada_server for run the metadata update
# only with he groupby module the metadata is not updated
dfs <- list('df_iris' = iris |> as.data.table(),
            'df_mtcars' = mtcars |> as.data.table())

temp_dir <- tempdir()

start_conf <- list(
  'conf_dir' = paste0(temp_dir, '\\conf'),
  'data_dir' = paste0(temp_dir, '\\data'),
  'theme' = 'spada_theme',
  'file_size' = 1000,
  'restore_session' = 'never',
  'save_session' = 'ask',
  'restore_data_status' = 0,
  'restore_output_status' = 0,
  'restore_status' = NULL,
  'plot_fill_color' = plot_fill_color,
  'plot_line_color' = plot_line_color
)

test_that('Test group by - overwrite - metadata update', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    groupby_server('groupby')

    session$setInputs(
      'groupby-vars_groupby' = c('cyl', 'am'),
      'groupby-vars_sel' = 'hp',
      'groupby-fun' = 'sum',
      'groupby-txt_new_name' = 'sum_hp',
      'groupby-radio_overwrite' = 'overwrite',
      'groupby-txt_new_dt_name' = 'new_dt'
    )
    session$setInputs('groupby-btn_add_var' = 1)

    session$setInputs(
      'groupby-vars_sel' = 'mpg',
      'groupby-fun' = 'mean',
      'groupby-txt_new_name'= 'mean_mpg'
    )
    session$setInputs('groupby-btn_add_var' = 2)

    session$setInputs('groupby-btn_groupby' = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(sum_hp = sum(hp),
             mean_mpg = mean(mpg)), by = c('cyl', 'am')]
    )

    expect_equal(
      session$userData$dt$meta[[session$userData$dt$act_name]],
      dt[, .(sum_hp = sum(hp),
             mean_mpg = mean(mpg)), by = c('cyl', 'am')] |> df_info()
    )

  })
})

# test group by - remove and group --------------------------------------------
test_that('Test group by - 1x1 var - overwrite - remove', {
  testServer(groupby_server, {

    session$userData$dt <- reactiveValues(
      dt = list('mtcars' = dt),
      act_name = 'mtcars',
      meta = list('mtcars' = mtcars |> df_info())
    )

    session$userData$dt_names <- reactive('mtcars')
    session$userData$dt$act_meta <- reactive(dt |> df_info())
    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_groupby = 'cyl',
      vars_sel = 'mpg',
      fun = 'min',
      txt_new_name = 'min_mpg',
      radio_overwrite = 'overwrite'
    )

    session$setInputs(btn_add_var = 1)

    session$setInputs(
      vars_sel = 'hp',
      fun = 'mean',
      txt_new_name = 'mean_hp'
    )
    session$setInputs(btn_add_var = 2)

    expect_equal(group$newvars, c('min_mpg', 'mean_hp'))
    expect_equal(group$funs, c('min', 'mean'))
    expect_equal(group$vars, c('mpg', 'hp'))

    session$setInputs(
      txt_new_name = 'min_mpg',
      btn_rm_var = 2
    )

    expect_equal(group$newvars, 'mean_hp')
    expect_equal(group$funs, 'mean')
    expect_equal(group$vars, 'hp')

    session$setInputs(btn_groupby = 1)

    expect_equal(
      session$userData$dt$dt[[session$userData$dt$act_name]],
      dt[, .(mean_hp = mean(hp)), by = 'cyl']
    )
  })
})
