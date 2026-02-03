# tests/testthat/test-spada_server.R

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

# test inputed datasets -------------------------------------------------------
test_that('Test inputed datasets', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$dt[[1]], iris |> as.data.table())
    expect_equal(session$userData$dt$dt[[2]], mtcars |> as.data.table())
    expect_equal(names(session$userData$dt$dt), c('df_iris', 'df_mtcars'))
    expect_equal(session$userData$dt_names(), c('df_iris', 'df_mtcars'))
  })
})

# test check datasets classes -------------------------------------------------
test_that('Test inputed datasets - classes', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$dt[[1]] |> class(), c('data.table', 'data.frame'))
    expect_equal(session$userData$dt$dt[[2]] |> class(), c('data.table', 'data.frame'))
  })
})

# test active dt --------------------------------------------------------------
test_that('Test active dt', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$act_name, 'df_iris')
    expect_equal(session$userData$dt$dt[[session$userData$dt$act_name]],
                 dfs[['df_iris']] |> as.data.table())
    expect_equal(session$userData$dt$bkp, NULL)
  })
})

# test active dt metadata -----------------------------------------------------
test_that('Test active dt metadata', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {
    expect_equal(session$userData$dt$act_meta(), df_info(iris))
  })
})

# test metadata - dt info -----------------------------------------------------
test_that('Test metadata - df_info', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    expect_equal(df_info(session$userData$dt$dt[[session$userData$dt$act_name]]) |> nrow(), 5)
    expect_equal(df_info(session$userData$dt$dt[[session$userData$dt$act_name]]), df_info(iris))
  })
})

# test metadata - dt info -----------------------------------------------------
test_that('Test metadata - gt class', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    expect_true('gt_tbl' %in% class(session$userData$dt$act_meta() |>
                                      gt_info(df_name = names(dfs)[2])))

  })
})

# test metadata - gt metadata -------------------------------------------------
test_that('Test metadata - gt metadata', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    mtcars_meta <- mtcars |> df_info() |> gt_info(df_name = 'df_mtcars')
    session$userData$dt$act_name <- 'df_mtcars'

    gt_temp <- session$userData$dt$act_meta() |> gt_info(df_name = names(dfs)[2])

    expect_equal(gt_temp$`_data`, mtcars_meta$`_data`)
    expect_equal(gt_temp$`_data` |> nrow(), 11)
    expect_equal(gt_temp$`_data`$var, names(mtcars))
    expect_equal(gt_temp$`_data`$type, sapply(mtcars, typeof) |> unname())
    expect_equal(gt_temp$`_data`$class, sapply(mtcars, class) |> unname())
  })
})

# test metadata - calculate cols ----------------------------------------------
test_that('Test metadata - calculate cols', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_iris'

    calculate_cols_server('calculate_cols')

    session$setInputs(
      `calculate_cols-vars_sel` = 'Sepal.Length',
      `calculate_cols-txt_new_name_fun` = 'mean_Sepal.Length',
      `calculate_cols-fun` = 'mean',
      `calculate_cols-btn_apply_fun` = 1
    )

    iris2 <- iris |> as.data.table()
    iris2[, mean_Sepal.Length := mean(Sepal.Length)]

    expect_equal(get_act_dt(session), iris2)
    expect_equal(session$userData$dt$act_meta()[['var']], names(iris2))

    expect_equal(session$userData$dt$act_meta(), iris2 |> df_info())

    session$userData$dt$act_name <- 'df_mtcars'

    expect_equal(get_act_dt(session), mtcars |> as.data.table())
    expect_equal(session$userData$dt$act_meta(), mtcars |> df_info())
  })
})

# test metadata - convert cols ------------------------------------------------
test_that('Test metadata - convert cols', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_iris'

    convert_cols_server('convert_cols')

    session$setInputs(
      'convert_cols-vars_sel' = 'Sepal.Length',
      'convert_cols-sel_format' = 'as.integer',
      'convert_cols-btn_apply' = 1
    )

    iris2 <- iris |> as.data.table()
    iris2[, Sepal.Length := as.integer(Sepal.Length)]

    expect_equal(get_act_dt(session), iris2)
    expect_equal(session$userData$dt$act_meta()[['var']], names(iris2))

    expect_equal(session$userData$dt$act_meta(), iris2 |> df_info())

    session$userData$dt$act_name <- 'df_mtcars'

    expect_equal(get_act_dt(session), mtcars |> as.data.table())
    expect_equal(session$userData$dt$act_meta(), mtcars |> df_info())
  })
})

# test metadata - filter rows -------------------------------------------------
test_that('Test metadata - filter rows', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    filter_rows_server('filter_rows')

    session$setInputs(
      'filter_rows-one_var_sel' = 'hp',
      'filter_rows-filter_type' = 'one',
      'filter_rows-one_var_operator' = '>',
      'filter_rows-one_var_value' = 100,
      'filter_rows-btn_filter' = 1
    )

    mtcars2 <- mtcars |> as.data.table() |> subset(hp > 100)

    expect_equal(get_act_dt(session), mtcars2)
    expect_equal(session$userData$dt$act_meta()[['var']], names(mtcars2))
    expect_equal(session$userData$dt$act_meta(), mtcars2 |> df_info())

    session$userData$dt$act_name <- 'df_iris'

    expect_equal(get_act_dt(session), iris |> as.data.table())
    expect_equal(session$userData$dt$act_meta(), iris |> df_info())
  })
})

# test metadata - select cols -------------------------------------------------
test_that('Test metadata - selet cols - keep', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    select_cols_server('select_cols')

    session$setInputs(
      'select_cols-vars_sel' = 'mpg',
      'select_cols-radio_var_sel' = 'keep',
      'select_cols-btn_sel' = 1
    )

    mtcars2 <- mtcars |> as.data.table() |> subset(select = mpg)
    expect_equal(get_act_dt(session), mtcars2)
    expect_equal(session$userData$dt$act_meta()[['var']], 'mpg')
    expect_equal(session$userData$dt$act_meta(), mtcars2 |> df_info())

    session$userData$dt$act_name <- 'df_iris'
    expect_equal(get_act_dt(session), iris |> as.data.table())
    expect_equal(session$userData$dt$act_meta(), iris |> df_info())
  })
})

# test metadata - order rows --------------------------------------------------
test_that('Test metadata - order rows', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    order_rows_server('order_rows')

    session$setInputs(
      'order_rows-vars_rows' = 'hp',
      'order_rows-radio_nas' = 'last',
      'order_rows-btn_order_rows' = 1
    )
    expect_equal(get_act_dt(session), mtcars[order(mtcars$hp), ] |> as.data.table())
    expect_equal(session$userData$dt$act_meta()[['var']], names(mtcars))
    expect_equal(session$userData$dt$act_meta(), mtcars |> df_info())

  })
})

# test metadata - reorder only ------------------------------------------------
test_that('Test metadata - order_cols after', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    order_cols_server('order_cols')

    session$setInputs(
      'order_cols-vars_cols' = 'cyl',
      'order_cols-vars_rest' = 'gear',
      'order_cols-radio_cols' = 'after',
      'order_cols-btn_order_cols' = 1
    )

    vars <- c('cyl')
    var1 <- names(mtcars)[10]

    mtcars_reordered <- mtcars |>
      dplyr::relocate(tidyselect::all_of(vars), .after = tidyselect::all_of(var1))

    expect_equal(get_act_dt(session), mtcars_reordered |> as.data.table())
    expect_equal(session$userData$dt$act_meta()[['var']], names(mtcars_reordered))
    expect_equal(session$userData$dt$act_meta(), mtcars_reordered |> df_info())

  })
})

# test metadata - rename only -------------------------------------------------
test_that('Test metadata - rename 1 col', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$dt$act_name <- 'df_mtcars'

    rename_cols_server('rename_cols')

    session$setInputs(
      'rename_cols-vars_sel' = 'cyl',
      'rename_cols-txt_new_name' = 'cyl2',
      'rename_cols-btn_rename' = 1
    )

    df <- mtcars
    names(df)[2] <- 'cyl2'

    expect_equal(session$userData$dt$act_meta()[['var']], names(df))

    session$userData$dt$act_name <- 'df_iris'

    session$setInputs(
      'rename_cols-vars_sel' = 'Species',
      'rename_cols-txt_new_name' = 'Species2',
      'rename_cols-btn_rename' = 2
    )

    df <- iris
    names(df)[5] <- 'Species2'

    expect_equal(get_act_dt(session), df |> as.data.table())
    expect_equal(session$userData$dt$act_meta()[['var']], names(df))
    expect_equal(session$userData$dt$act_meta(), df |> df_info())

  })
})
