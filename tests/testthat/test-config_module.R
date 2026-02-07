# tests/testthat/test-config_module.R

# test colors -----------------------------------------------------------------
test_that('Test chose colors', {
  testServer(config_server, {

    session$setInputs(sel_fill = '#aaccff')
    session$setInputs(sel_line = '#ccee55')

    expect_equal(palette()[[1]], '#aaccff')
    expect_equal(palette()[[2]], '#ccee55')
  })
})

# test input file size --------------------------------------------------------

test_that('Test input file size', {
  testServer(config_server, {

    session$userData$conf$conf_dir <- reactiveValues(conf_dir = tempdir())

    session$setInputs(input_file_size = 1, btn_file_size = 1)

    expect_equal(max_request_size(), 1 * 1024 ^ 2)
  })
})
