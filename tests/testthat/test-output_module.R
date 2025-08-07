# tests/testthat/test-output_module.R

out_el <- list(
  report_card(
    title = 'Test element 1',
    annotation = 'Annotation test 1',
    content = tags$p('Element 1')
  ),
  report_card(
    title = 'Test element 2',
    annotation = 'Annotation test 2',
    content = tags$p('Element 2')
  )
)

# test elements ---------------------------------------------------------------
test_that('Reset elements in output', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)

    expect_length(session$userData$out$elements, 2)
    expect_true(inherits(session$userData$out$elements[[1]], 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]], 'shiny.tag'))
  })
})


# test btn reset --------------------------------------------------------------
test_that('Reset button clears output', {
  testServer(output_server, {

    session$userData$out <- reactiveValues(elements = out_el)

    session$setInputs(btn_reset = 1)
    session$setInputs(btn_confirm_reset = 1)

    expect_length(session$userData$out$elements, 1)
    expect_true(inherits(session$userData$out$elements[[1]], 'shiny.tag'))
  })
})

# test save output ------------------------------------------------------------
test_that('Save output stores RDS file', {
  testServer(output_server, {

    tmpdir <- tempdir()

    session$userData$conf <- reactiveValues(data_dir = tmpdir)
    session$userData$out <- reactiveValues(elements = out_el)

    session$setInputs(btn_save_output_session = 1)
    session$setInputs(btn_confirm_save_output = 1)

    saved_file <- file.path(tmpdir, 'output.RDS')
    expect_true(file.exists(saved_file))

    saved_content <- readRDS(saved_file)
    expect_true(is.list(saved_content))
    expect_length(saved_content, 2)
    expect_true(inherits(saved_content[[1]], 'shiny.tag'))
    expect_true(inherits(saved_content[[2]], 'shiny.tag'))
  })
})

# test import output ----------------------------------------------------------
test_that('Import output loads RDS file correctly', {
  testServer(output_server, {

    tmpdir <- tempdir()
    saveRDS(out_el, file.path(tmpdir, 'output.RDS'))

    session$userData$conf <- reactiveValues(data_dir = tmpdir)
    session$userData$out <- reactiveValues(elements = list())

    session$setInputs(btn_import_output_session = 1)
    session$setInputs(btn_confirm_import_output = 1)

    expect_length(session$userData$out$elements, 2)
    expect_equal(session$userData$out$elements, out_el)
    expect_true(inherits(session$userData$out$elements[[1]], 'shiny.tag'))
    expect_true(inherits(session$userData$out$elements[[2]], 'shiny.tag'))
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
