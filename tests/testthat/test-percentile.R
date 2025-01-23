test_that("pn versus quantile", {
  expect_equal(pn(1:10, p = 0.5), quantile(1:10, probs = 0.5))
})

test_that("p10 versus quantile", {
  expect_equal(p10(1:10), quantile(1:10, probs = 0.1))
})

test_that("p25 versus quantile", {
  expect_equal(p25(1:10), quantile(1:10, probs = 0.25))
})

test_that("p75 versus quantile", {
  expect_equal(p75(1:10), quantile(1:10, probs = 0.75))
})

test_that("p90 versus quantile", {
  expect_equal(p90(1:10), quantile(1:10, probs = 0.9))
})

test_that("p95versus quantile", {
  expect_equal(p95(1:10), quantile(1:10, probs = 0.95))
})

test_that("p99 versus quantile", {
  expect_equal(p99(1:10), quantile(1:10, probs = 0.99))
})
