# tests/testthat/test-data_overview_module.R


# test validate number of rows ------------------------------------------------
test_that("Test sample rows", {
  testServer(data_overview_server, args = list(df = reactive(iris)), {
    session$setInputs(size_sample = 0)
    expect_error(idx(), "Number of rows must be > 0")
  })
})

# test first rows -------------------------------------------------------------
test_that("Test first rows", {
  testServer(data_overview_server, args = list(df = reactive(iris)), {

    # Simulate inputs
    session$setInputs(size_sample = 5)
    session$setInputs(radio_sample = "first")

    expect_equal(data_gt(), iris[1:5,])

  })
})

# test sample of rows ---------------------------------------------------------
test_that("Test sample rows", {
  testServer(data_overview_server, args = list(df = reactive(iris)), {
    # Simulate inputs
    session$setInputs(size_sample = 5)
    session$setInputs(radio_sample = "sample")

    # test for equality (unlist to avoid error caused by row.names)
    expect_equal(data_gt() |> unlist(),
                 iris[idx(),] |> unlist())
  })
})
