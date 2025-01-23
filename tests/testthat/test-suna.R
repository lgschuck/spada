test_that('suna - works with numeric vector without NA values', {
  expect_equal(suna(c(1, 2, 3)), 6)
})

test_that('suna - ignores NA values when na_rm = TRUE (default)', {
  expect_equal(suna(c(NA, 2, 3)), 5)
})

test_that('suna - returns NA if na_rm = FALSE and there are NA values in the vector', {
  expect_true(is.na(suna(c(NA, 2, 3), na_rm = FALSE)))
})

test_that('suna - returns 0 for an empty vector', {
  expect_equal(suna(numeric(0)), 0)
})

test_that('suna - returns the correct value for a vector with a single element', {
  expect_equal(suna(5), 5)
})

test_that('suna - calculates the sum for a vector with negative values', {
  expect_equal(suna(c(-1, -2, -3)), -6)
})

test_that('suna - calculates the sum for a vector with positive, negative, and zero values', {
  expect_equal(suna(c(-1, 0, 1)), 0)
})

test_that('suna - works with logical vectors (TRUE and FALSE treated as 1 and 0)', {
  expect_equal(suna(c(TRUE, FALSE, TRUE)), 2)
})

test_that('suna - throws an error when given a character vector', {
  expect_error(suna(c('a', 'b', 'c')))
})
