test_that('mana - all valid values', {
  expect_equal(mana(c(1, 2, 3)), 3)
})

test_that('mana - all NA values', {
  expect_equal(mana(c(NA, NA, NA)), NA)
})

test_that('mana - all NaN values', {
  expect_equal(mana(c(NaN, NaN, NaN)), NA)
})

test_that('mana - only n 1 valid out of 3', {
  expect_equal(mana(c(1, NA, NA)), 1)
})

test_that('mana - only n 2 valid out of 3', {
  expect_equal(mana(c(NA, 2, NA)), 2)
})

test_that('mana - only n 3 valid out of 3', {
  expect_equal(mana(c(NA, NA, 3)), 3)
})

test_that('mana - 2 and 3 valid out of 3', {
  expect_equal(mana(c(NA, 2, 3)), 3)
})

test_that('mana - NA, 3, 2', {
  expect_equal(mana(c(NA, 3, 2)), 3)
})
