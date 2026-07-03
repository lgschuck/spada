# tests/testthat/test-spada.R

# test spada - reject invalid inputs ------------------------------------------
test_that('spada() reject invalid inputs', {
  expect_error(spada(data.frame()),
               'Objects must be data.frame and have at least 1 row and 1 col each')
  expect_error(spada(iris, list()),
               'Objects must be data.frame and have at least 1 row and 1 col each')
  expect_error(spada(iris, data.frame()),
               'Objects must be data.frame and have at least 1 row and 1 col each')
  expect_error(spada(iris, x = 1:10),
               'Objects must be data.frame and have at least 1 row and 1 col each')
  expect_error(spada(data.frame(Var = character(0))),
               'Objects must be data.frame and have at least 1 row and 1 col each')
  daemons(0)
})
