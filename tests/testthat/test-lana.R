test_that('lana - all valid values', {
  expect_equal(lana(c(1, 2, 3)), 3)
})

test_that('lana - all NA values', {
  expect_equal(lana(c(NA, NA, NA)), NA)
})

test_that('lana - all NaN values', {
  expect_equal(lana(c(NaN, NaN, NaN)), NA)
})

test_that('lana - only n 1 valid out of 3', {
  expect_equal(lana(c(1, NA, NA)), 1)
})

test_that('lana - only n 2 valid out of 3', {
  expect_equal(lana(c(NA, 2, NA)), 2)
})

test_that('lana - only n 3 valid out of 3', {
  expect_equal(lana(c(NA, NA, 3)), 3)
})

test_that('lana - 2 and 3 valid out of 3', {
  expect_equal(lana(c(NA, 2, 3)), 3)
})
