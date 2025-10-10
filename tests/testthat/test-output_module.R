# tests/testthat/test-output_module.R

out_el <- list(
  'id_1' = list(
    'id' = 'id_1',
    'title' = 'Title 1',
    'card' = report_card(
        title = 'Test element 1',
        annotation = 'Annotation test 1',
        content = tags$p('Element 1')
    )
  ),
  'id_2' = list(
    'id' = 'id_2',
    'title' = 'Title 2',
    'card' = report_card(
      title = 'Test element 2',
      annotation = 'Annotation test 2',
      content = tags$p('Element 2')
    )
  )
)

# test elements ---------------------------------------------------------------
test_that('Reset elements in output', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)

    expect_length(session$userData$out$elements, 2)
    expect_true(inherits(session$userData$out$elements[[1]], 'list'))
    expect_true(inherits(session$userData$out$elements[[2]], 'list'))

    # test id
    expect_true(inherits(session$userData$out$elements[[1]][[1]], 'character'))
    expect_true(inherits(session$userData$out$elements[[1]][[2]], 'character'))

    # test title
    expect_true(inherits(session$userData$out$elements[[2]][[1]], 'character'))
    expect_true(inherits(session$userData$out$elements[[2]][[2]], 'character'))

    # test card
    expect_true(inherits(session$userData$out$elements[[1]][[3]], 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]][[3]], 'shiny.tag'))

  })
})

# test btn reset --------------------------------------------------------------
test_that('Reset button clears output', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)

    session$setInputs(btn_reset = 1)
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
    expect_true(inherits(saved_content[[1]][[1]], 'character'))
    expect_true(inherits(saved_content[[1]][[2]], 'character'))

    # test title
    expect_true(inherits(saved_content[[2]][[1]], 'character'))
    expect_true(inherits(saved_content[[2]][[2]], 'character'))

    # test card
    expect_true(inherits(saved_content[[1]][[3]], 'shiny.tag'))
    expect_true(inherits(saved_content[[2]][[3]], 'shiny.tag'))
  })
})

# test import output ----------------------------------------------------------
test_that('Import output loads qs2 file correctly', {
  testServer(output_server, {

    tmpdir <- tempdir()
    qs_save(out_el, file.path(tmpdir, 'output.qs2'))

    session$userData$conf <- reactiveValues(data_dir = tmpdir)
    session$userData$out <- reactiveValues(elements = list())

    session$setInputs(btn_import_output_session = 1)
    session$setInputs(btn_confirm_import_output = 1)

    expect_length(session$userData$out$elements, 2)
    expect_equal(session$userData$out$elements, out_el)
    expect_true(inherits(session$userData$out$elements[[1]], 'list'))
    expect_true(inherits(session$userData$out$elements[[2]], 'list'))

    # test id
    expect_true(inherits(session$userData$out$elements[[1]][[1]], 'character'))
    expect_true(inherits(session$userData$out$elements[[1]][[2]], 'character'))

    # test title
    expect_true(inherits(session$userData$out$elements[[2]][[1]], 'character'))
    expect_true(inherits(session$userData$out$elements[[2]][[2]], 'character'))

    # test card
    expect_true(inherits(session$userData$out$elements[[1]][[3]], 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]][[3]], 'shiny.tag'))
  })
})

# test download html ----------------------------------------------------------
test_that('Download HTML ', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)
    expect_equal(
      output$btn_save_html |> basename(),
      paste0('output_',
             format(Sys.time(), format = '%Y%m%d%H%M%S'),
             '.html')
    )
  })
})
