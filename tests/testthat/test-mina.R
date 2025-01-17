test_that('mina - all valid values', {
  expect_equal(mina(c(1, 2, 3)), 1)
})

test_that('mina - all NA values', {
  expect_equal(mina(c(NA, NA, NA)), NA)
})

test_that('mina - all NaN values', {
  expect_equal(mina(c(NaN, NaN, NaN)), NA)
})

test_that('mina - only n 1 valid out of 3', {
  expect_equal(mina(c(1, NA, NA)), 1)
})

test_that('mina - only n 2 valid out of 3', {
  expect_equal(mina(c(NA, 2, NA)), 2)
})

test_that('mina - only n 3 valid out of 3', {
  expect_equal(mina(c(NA, NA, 3)), 3)
})

test_that('mina - 2 and 3 valid out of 3', {
  expect_equal(mina(c(NA, 2, 3)), 2)
})

test_that('mina - NA, 3, 2', {
  expect_equal(mina(c(NA, 3, 2)), 2)
})
