# tests/testthat/test-about_spada_module.R

# test session info -----------------------------------------------------------
test_that("Test session info", {

  testServer(about_spada_server, {

    session_output <- output$session_info
    expect_type(session_output, "character")
    expect_true(grepl("R version 4.4.2", session_output))
  })
})

# test info about spada -------------------------------------------------------
test_that("Test info about spada", {

  testServer(about_spada_server, {

    package_output <- output$info_about_spada
    expect_type(package_output, "character")
    expect_true(grepl("Package: spada", package_output))
    expect_true(grepl("Shiny Package for Data Analysis", package_output))
    expect_true(grepl("Depends: R (>= 4.4.0)", package_output, fixed = T))
  })
})

# test dependencies -----------------------------------------------------------
test_that("Test dependencies", {

  testServer(about_spada_server, {

    package_output <- output$info_about_spada
    expect_true(grepl("bsicons", package_output, fixed = T))
    expect_true(grepl("bslib", package_output, fixed = T))
    expect_true(grepl("data.table", package_output, fixed = T))
    expect_true(grepl("DescTools", package_output, fixed = T))
    expect_true(grepl("dplyr", package_output, fixed = T))
    expect_true(grepl("ggplot2", package_output, fixed = T))
    expect_true(grepl("gt", package_output, fixed = T))
    expect_true(grepl("haven", package_output, fixed = T))
    expect_true(grepl("htmltools", package_output, fixed = T))
    expect_true(grepl("rlang", package_output, fixed = T))
    expect_true(grepl("sass", package_output, fixed = T))
    expect_true(grepl("shiny", package_output, fixed = T))
    expect_true(grepl("shinybusy", package_output, fixed = T))
    expect_true(grepl("shinyWidgets", package_output, fixed = T))
  })
})

