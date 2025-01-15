test_that("is_date - test Sys.Date()", {
  expect_equal(is_date(Sys.Date()), TRUE)
})

test_that("is_date - test Sys.time()", {
  expect_equal(is_date(Sys.time()), TRUE)
})

test_that("is_date - test numeric", {
  expect_equal(is_date(10), FALSE)
})

test_that("is_date - test char", {
  expect_equal(is_date('20'), FALSE)
})

test_that("is_date - test char in Date format", {
  expect_equal(is_date('2024-12-01'), FALSE)
})
