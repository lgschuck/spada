# tests/testthat/test-spada.R

# test spada - input data.frames ----------------------------------------------
test_that('spada() rejects empty data.frame', {
  expect_error(spada(data.frame()),
               'Objects must be data.frame and have at least 1 row and 1 col each')
})

test_that('spada() rejects valid data.frame plus invalid object', {
  expect_error(spada(iris, list()),
               'Objects must be data.frame and have at least 1 row and 1 col each')
})

test_that('spada() rejects valid data.frame plus empty data.frame', {
  expect_error(spada(iris, data.frame()),
               'Objects must be data.frame and have at least 1 row and 1 col each')
})

test_that('spada() rejects valid data.frame plus a vector', {
  expect_error(spada(iris, x = 1:10),
               'Objects must be data.frame and have at least 1 row and 1 col each')
})

test_that('spada() rejects zero row data.frame', {
  expect_error(spada(data.frame(Var = character(0))),
               'Objects must be data.frame and have at least 1 row and 1 col each')
})

