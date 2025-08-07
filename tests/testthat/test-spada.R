# tests/testthat/test-spada.R

# test spada - input data.frames ----------------------------------------------
test_that('spada() rejects invalid inputs', {
  expect_error(spada(data.frame(), list()),
               'Objects must be data.frame and have at least 1 row each')

  expect_error(spada(data.frame(), iris),
               'Objects must be data.frame and have at least 1 row each')

  expect_error(spada(iris, x = 1:10),
               'Objects must be data.frame and have at least 1 row each')
})
