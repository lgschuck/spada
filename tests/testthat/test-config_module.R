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

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$userData$conf$conf_dir <- reactiveValues(conf_dir = tempdir())

    session$setInputs(input_file_size = 1, btn_file_size = 1)

    expect_equal(max_request_size(), 1 * 1024 ^ 2)
    expect_equal(last_msg, 'New limit applied')
  })
})

# test theme choice -----------------------------------------------------------
test_that('Test theme choice', {
  testServer(config_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$setInputs(theme_choice = 'spada_dark_theme', btn_theme = 1)

    expect_equal(session$userData$conf$theme, 'spada_dark_theme')
    expect_equal(last_msg, 'New theme applied')

  })
})

# test restore session settings -----------------------------------------------
test_that('Test restore session settings', {
  testServer(config_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$setInputs(radio_restore_session = 'always',
                      radio_save_session = 'ask',
                      btn_save_session_conf = 1)

    expect_equal(session$userData$conf$restore_session, 'always')
    expect_equal(session$userData$conf$save_session, 'ask')
    expect_equal(last_msg, 'New settings applied')

  })
})

# test plot limit -------------------------------------------------------------
test_that('Test plot limit ', {
  testServer(config_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    session$setInputs(input_plot_limit = 100, btn_plot_limit = 1)

    expect_equal(session$userData$conf$plot_limit, 100e3)
    expect_equal(last_msg, 'New limit applied')

    session$setInputs(input_plot_limit = -100, btn_plot_limit = 2)
    expect_equal(last_msg, 'Value must be > 0')
  })
})


