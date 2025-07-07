# tests/testthat/test-gt_info.R

# test gt info ----------------------------------------------------------------
test_that('Test gt info', {

  gt_info_test <- iris |> df_info() |> gt_info(df_name = 'iris')
  expect_equal(gt_info_test |> class(), c('gt_tbl', 'list'))
  expect_equal(gt_info_test |> length(), 18)

  expect_equal(gt_info_test$`_data`$var, names(iris))
  expect_equal(gt_info_test$`_data`$type, sapply(iris, typeof) |> unname())
  expect_equal(gt_info_test$`_data`$class, sapply(iris, class) |> unname())
  expect_equal(gt_info_test$`_data`$rows, rep(150, 5))
  expect_equal(gt_info_test$`_data`$cols, rep(5, 5))
})

