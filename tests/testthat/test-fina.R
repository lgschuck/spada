test_that('fina - all valid values', {
  expect_equal(fina(c(1, 2, 3)), 1)
})

test_that('fina - all NA values', {
  expect_equal(fina(c(NA, NA, NA)), NA)
})

test_that('fina - all NaN values', {
  expect_equal(fina(c(NaN, NaN, NaN)), NA)
})

test_that('fina - only n 1 valid out of 3', {
  expect_equal(fina(c(1, NA, NA)), 1)
})

test_that('fina - only n 2 valid out of 3', {
  expect_equal(fina(c(NA, 2, NA)), 2)
})

test_that('fina - only n 3 valid out of 3', {
  expect_equal(fina(c(NA, NA, 3)), 3)
})

test_that('fina - 2 and 3 valid out of 3', {
  expect_equal(fina(c(NA, 2, 3)), 2)
})
