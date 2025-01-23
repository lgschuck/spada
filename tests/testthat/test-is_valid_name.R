test_that("is_valid_name - test a valid name", {
  expect_equal(is_valid_name('name'), TRUE)
})

test_that("is_valid_name - test 2 size vector with valid", {
  expect_equal(is_valid_name(c('a', 'b')), c(TRUE, TRUE))
})

test_that("is_valid_name - test name with space", {
  expect_equal(is_valid_name('a a'), FALSE)
})

test_that("is_valid_name - test name starting with number", {
  expect_equal(is_valid_name('1a'), FALSE)
})

test_that("is_valid_name - test name starting with blank space", {
  expect_equal(is_valid_name(' 1a'), FALSE)
})

