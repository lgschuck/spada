test_that('mina - all valid values', {
  expect_equal(mina(c(1, 2, 3)), 1)
})

test_that('mina - ignores NA when na_rm = TRUE', {
  expect_equal(mina(c(NA, 2, 3)), 2)
})

test_that('mina returns NA if na_rm = FALSE', {
  expect_true(is.na(mina(c(NA, 2, 3), na_rm = FALSE)))
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

test_that('mina - test is.na all NA', {
  expect_true(is.na(mina(c(NA, NA, NA))))
})

test_that('mina - works with logical vectors', {
  expect_equal(mina(c(TRUE, FALSE, TRUE)), 0)
})

test_that('mina - with character vector', {
  expect_equal(mina(c('a', 'b', 'c')), 'a')
})
