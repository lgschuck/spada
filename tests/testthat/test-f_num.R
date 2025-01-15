test_that("f_num - default billion test", {
  expect_equal(f_num(12345678956), '12 B')
})

test_that("f_num - default million test", {
  expect_equal(f_num(123456789), '123 M')
})

test_that("f_num - default thousand test", {
  expect_equal(f_num(1234), '1 K')
})

test_that("f_num - default Inf test", {
  expect_equal(f_num(Inf), 'Inf')
})

test_that("f_num - numeric Inf test", {
  expect_equal(f_num(100^308), 'Inf')
})

test_that("f_num - numeric -Inf test", {
  expect_equal(f_num(-100^308), '-Inf')
})
