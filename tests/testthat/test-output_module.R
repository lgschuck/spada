# tests/testthat/test-output_module.R

out_el <- list(
  'id_1' = gen_output(),
  'id_2' = gen_output()
)

# test elements ---------------------------------------------------------------
test_that('Test output structure', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)

    expect_length(session$userData$out$elements, 2)
    expect_true(inherits(session$userData$out$elements[[1]], 'list'))
    expect_true(inherits(session$userData$out$elements[[2]], 'list'))

    # test id
    expect_true(inherits(session$userData$out$elements[[1]]$id, 'character'))
    expect_true(inherits(session$userData$out$elements[[2]]$id, 'character'))

    # test title
    expect_true(inherits(session$userData$out$elements[[1]]$title, 'character'))
    expect_true(inherits(session$userData$out$elements[[2]]$title, 'character'))

    # test annotation
    expect_true(inherits(session$userData$out$elements[[1]]$annotation, 'character'))
    expect_true(inherits(session$userData$out$elements[[2]]$annotation, 'character'))

    # test element
    expect_true(inherits(session$userData$out$elements[[1]]$element, c('shiny.tag', 'gt_tbl')))
    expect_true(inherits(session$userData$out$elements[[2]]$element, c('shiny.tag', 'gt_tbl')))

    # test card
    expect_true(inherits(session$userData$out$elements[[1]]$card, 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]]$card, 'shiny.tag'))

    # test btn_x
    expect_true(inherits(session$userData$out$elements[[1]]$btn_x, 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]]$btn_x, 'shiny.tag'))

    # test btn_e
    expect_true(inherits(session$userData$out$elements[[1]]$btn_e, 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]]$btn_e, 'shiny.tag'))

  })
})

# test btn reset --------------------------------------------------------------
test_that('Reset button clears output', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    expect_length(session$userData$out$elements, 2)
    session$setInputs(btn_reset = 1)

    expect_length(session$userData$out$elements, 2)
    session$setInputs(btn_confirm_reset = 1)

    expect_length(session$userData$out$elements, 0)
    expect_true(inherits(session$userData$out$elements, 'list'))
  })
})

# test save output ------------------------------------------------------------
test_that('Save output stores qs2 file', {
  testServer(output_server, {

    tmpdir <- tempdir()

    session$userData$conf <- reactiveValues(data_dir = tmpdir)
    session$userData$out <- reactiveValues(elements = out_el)
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    session$setInputs(btn_save_output_session = 1)
    session$setInputs(btn_confirm_save_output = 1)

    saved_file <- file.path(tmpdir, 'output.qs2')
    expect_true(file.exists(saved_file))

    saved_content <- qs_read(saved_file)
    expect_true(is.list(saved_content))
    expect_length(saved_content, 2)
    expect_true(inherits(saved_content[[1]], 'list'))
    expect_true(inherits(saved_content[[2]], 'list'))

    # test id
    expect_true(inherits(saved_content[[1]]$id, 'character'))
    expect_true(inherits(saved_content[[2]]$id, 'character'))

    # test title
    expect_true(inherits(saved_content[[1]]$title, 'character'))
    expect_true(inherits(saved_content[[2]]$title, 'character'))

    # test annotation
    expect_true(inherits(saved_content[[1]]$annotation, 'character'))
    expect_true(inherits(saved_content[[2]]$annotation, 'character'))

    # test element
    expect_true(inherits(saved_content[[1]]$element, c('shiny.tag', 'gt_tbl')))
    expect_true(inherits(saved_content[[2]]$element, c('shiny.tag', 'gt_tbl')))

    # test card
    expect_true(inherits(saved_content[[1]]$card, 'shiny.tag'))
    expect_true(inherits(saved_content[[2]]$card, 'shiny.tag'))

    # test btn_x
    expect_true(inherits(saved_content[[1]]$btn_x, 'shiny.tag'))
    expect_true(inherits(saved_content[[2]]$btn_x, 'shiny.tag'))

    # test btn_e
    expect_true(inherits(saved_content[[1]]$btn_e, 'shiny.tag'))
    expect_true(inherits(saved_content[[2]]$btn_e, 'shiny.tag'))

  })
})

# test import output ----------------------------------------------------------
test_that('Import output loads qs2 file correctly', {
  testServer(output_server, {

    tmpdir <- tempdir()
    qs_save(out_el, file.path(tmpdir, 'output.qs2'))

    session$userData$conf <- reactiveValues(data_dir = tmpdir)
    session$userData$out <- reactiveValues(elements = list())
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    session$setInputs(btn_import_output_session = 1)
    session$setInputs(btn_confirm_import_output = 1)

    expect_length(session$userData$out$elements, 2)
    expect_equal(
      grepl(pattern = 'id_', session$userData$out$elements),
      grepl(pattern = 'id_', out_el)
    )

    expect_true(inherits(session$userData$out$elements[[1]], 'list'))
    expect_true(inherits(session$userData$out$elements[[2]], 'list'))

    # test id
    expect_true(inherits(session$userData$out$elements[[1]]$id, 'character'))
    expect_true(inherits(session$userData$out$elements[[2]]$id, 'character'))

    # test title
    expect_true(inherits(session$userData$out$elements[[1]]$title, 'character'))
    expect_true(inherits(session$userData$out$elements[[2]]$title, 'character'))

    # test annotation
    expect_true(inherits(session$userData$out$elements[[1]]$annotation, 'character'))
    expect_true(inherits(session$userData$out$elements[[2]]$annotation, 'character'))

    # test element
    expect_true(inherits(session$userData$out$elements[[1]]$element, c('shiny.tag', 'gt_tbl')))
    expect_true(inherits(session$userData$out$elements[[2]]$element, c('shiny.tag', 'gt_tbl')))

    # test card
    expect_true(inherits(session$userData$out$elements[[1]]$card, 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]]$card, 'shiny.tag'))

    # test btn_x
    expect_true(inherits(session$userData$out$elements[[1]]$btn_x, 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]]$btn_x, 'shiny.tag'))

    # test btn_e
    expect_true(inherits(session$userData$out$elements[[1]]$btn_e, 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]]$btn_e, 'shiny.tag'))
  })
})

# test download html ----------------------------------------------------------
test_that('Download HTML ', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    expect_equal(
      output$btn_save_html |> basename() |> substr(1, 15),
      paste0(
        'output_',
        format(Sys.time(), format = '%Y%m%d%H%M%S'),
        '.html') |> substr(1, 15)
    )
  })
})

# test printable output -------------------------------------------------------
test_that('Test output - printable output', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    session$setInputs(sel_show_output = Inf)

    # wait for the extended_task
    while (task_printable_out$status() == 'running') {
      session$flushReact()
    }

    expect_equal(task_printable_out$status(), 'success')
    expect_s3_class(printable_output()[[1]], c('shiny.tag'))

    expect_true(
      all(
        sapply(
          printable_output(),
          \(x){ class(x) == 'shiny.tag' }
        )
      )
    )
  })
})

# test edit output element ----------------------------------------------------
dfs <- list('df_iris' = iris |> as.data.table(),
            'df_mtcars' = mtcars |> as.data.table())

temp_dir <- tempdir()

start_conf <- list(
  'conf_dir' = file.path(temp_dir, 'conf'),
  'data_dir' = file.path(temp_dir, 'data'),
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


test_that('Test output - edit element', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$out <- reactiveValues(elements = list())
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    out_el <- div(h2('Element'))

    insert_output_server('insert_output', reactive(out_el))
    output_server('output')

    # insert output
    session$setInputs(
      'insert_output-output_title' = 'Title 1',
      'insert_output-output_annot' = 'Annotation 1',
      'insert_output-btn_add_output' = 1,
      'insert_output-btn_confirm_add_output' = 1
    )

    expect_length(session$userData$out$elements, 1)
    expect_equal(session$userData$out$elements[[ 1 ]]$title, 'Title 1')
    expect_equal(session$userData$out$elements[[ 1 ]]$annotation, 'Annotation 1')

    # trigger btn edit
    id <- session$userData$out$elements[[ 1 ]]$id
    edit_btn <- list()
    edit_btn[[paste0('insert_output-btn_eout_', id)]] = 1
    do.call(session$setInputs, edit_btn)

    # edit output
    session$setInputs(
      'output-output_edit_title' = 'New title',
      'output-output_edit_annot' = 'New annotation',
      'output-btn_confirm_edit_output' = 1
    )

    expect_type(session$userData$out$elements, 'list')
    expect_length(session$userData$out$elements, 1)
    expect_equal(session$userData$out$elements[[ 1 ]]$title, 'New title')
    expect_equal(session$userData$out$elements[[ 1 ]]$annotation, 'New annotation')
  })
})


# test delete output element --------------------------------------------------
test_that('Test output - edit element', {
  testServer(spada_server(datasets = dfs, conf = start_conf), {

    session$userData$out <- reactiveValues(elements = list())
    session$userData$out_edit_trigger <- reactiveVal(NULL)

    out_el <- div(h2('Element'))

    insert_output_server('insert_output', reactive(out_el))
    output_server('output')

    # insert output
    session$setInputs(
      'insert_output-output_title' = 'Title 1',
      'insert_output-output_annot' = 'Annotation 1',
      'insert_output-btn_add_output' = 1,
      'insert_output-btn_confirm_add_output' = 1
    )

    expect_length(session$userData$out$elements, 1)
    expect_equal(session$userData$out$elements[[ 1 ]]$title, 'Title 1')
    expect_equal(session$userData$out$elements[[ 1 ]]$annotation, 'Annotation 1')

    # trigger btn edit
    id <- session$userData$out$elements[[ 1 ]]$id
    delete_btn <- list()
    delete_btn[[paste0('insert_output-btn_xout_', id)]] = 1
    do.call(session$setInputs, delete_btn)

    expect_type(session$userData$out$elements, 'list')
    expect_length(session$userData$out$elements, 0)
  })
})
