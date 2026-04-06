# tests/testthat/test-restore_session_module.R

dfs <- list('df1' = data.table(x = 1:5),
            'df2' = data.table(y = letters[1:20]))

temp_dir <- tempdir()
dir.create(file.path(temp_dir, 'data'))

start_conf <- list(
  'empty_datasets' = 1,
  'conf_dir' = file.path(temp_dir, 'conf'),
  'data_dir' = file.path(temp_dir, 'data'),
  'theme' = 'spada_theme',
  'file_size' = 1000,
  'restore_session' = 'always',
  'save_session' = 'ask',
  'restore_data_status' = 0,
  'restore_output_status' = 0,
  'restore_status' = NULL,
  'plot_fill_color' = plot_fill_color,
  'plot_line_color' = plot_line_color
)

# test restore session - no data no output ------------------------------------
test_that('Test restore session no data and no output ', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    restore_session_server('restore_session')

    session$flushReact()
    expect_equal(session$userData$conf$restore_data_status, 2)
    expect_equal(session$userData$conf$restore_output_status, 2)
    expect_equal(session$userData$conf$restore_status, '2.2')

  })
})

# test restore session - load data and output ---------------------------------
output_for_test <- list('id_1' = gen_output())

qs_save(dfs, file.path(temp_dir, 'data', 'data.qs2'))
qs_save(output_for_test, file.path(temp_dir, 'data', 'output.qs2'))

test_that('Test restore session load data and output ', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    restore_session_server('restore_session')

    session$flushReact()
    expect_equal(session$userData$conf$restore_data_status, 1)
    expect_equal(session$userData$conf$restore_output_status, 1)
    expect_equal(session$userData$conf$restore_status, '1.1')

    expect_equal(session$userData$dt$dt |> names(), c('df1', 'df2'))
    expect_equal(session$userData$dt$dt, dfs)
    expect_equal(session$userData$out$elements[[1]]$title,
                 output_for_test[[1]]$title)
    expect_equal(session$userData$out$elements[[1]]$annotation,
                 output_for_test[[1]]$annotation)
    expect_equal(session$userData$out$elements[[1]]$element,
                 output_for_test[[1]]$element)
    expect_equal(session$userData$out$elements[[1]]$card,
                 output_for_test[[1]]$card)
    expect_equal(session$userData$out$elements[[1]]$btn_x |> class(),
                 output_for_test[[1]]$btn_x |> class())
    expect_s3_class(session$userData$out$elements[[1]]$btn_x, 'shiny.tag')
    expect_equal(session$userData$out$elements[[1]]$btn_e |> class(),
                 output_for_test[[1]]$btn_e |> class())
    expect_s3_class(session$userData$out$elements[[1]]$btn_e, 'shiny.tag')
  })
})

# test restore session - invalid format data and output -----------------------
qs_save(list(1:5), file.path(temp_dir, 'data', 'data.qs2'))
qs_save(list(letters), file.path(temp_dir, 'data', 'output.qs2'))

test_that('Test restore session load data and output ', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    restore_session_server('restore_session')

    session$flushReact()
    expect_equal(session$userData$conf$restore_data_status, 3)
    expect_equal(session$userData$conf$restore_output_status, 3)
    expect_equal(session$userData$conf$restore_status, '3.3')
  })
})
