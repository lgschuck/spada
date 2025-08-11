# tests/testthat/test-utils.R

# test filter_rows function ---------------------------------------------------
test_that('filter_rows function', {
  df <- data.frame(x = c(1, 2, 3, NA, NA)) |> as.data.table()

  expect_equal(nrow(filter_rows(df, 'x', '==', 2)), 1)
  expect_equal(nrow(filter_rows(df, 'x', 'is_na', NULL)), 2)
})

# test filter_rows 2 vars function --------------------------------------------
test_that('filter_rows 2 vars function', {
  df <- data.frame(x = c(1, 2, 3, NA, NA),
                   y = c(5:1)) |> as.data.table()

  df_filtered <- filter_rows_2vars(df, 'x', 'y', '==')

  expect_equal(nrow(df_filtered), 1)
  expect_equal(df_filtered[1, ]$y, 3)
  expect_equal(df_filtered[1, ]$x, 3)

  df_filtered <- filter_rows_2vars(df, 'x', 'y', '<=')

  expect_equal(nrow(df_filtered), 3)
  expect_equal(df_filtered$x, c(1, 2, 3))
  expect_equal(df_filtered$y, c(5, 4, 3))
})

test_that('filter_rows 2 vars function - wrong operator', {
  df <- data.frame(x = 1, y = 2) |> as.data.table()
  expect_error(filter_rows_2vars(df, 'x', 'y', 'xxx'))
})

# test is hex color function --------------------------------------------------
test_that('is_hex_color validates hex codes correctly', {
  expect_true(is_hex_color('#FFFFFF'))
  expect_false(is_hex_color('123456'))
})

# test make names append list function ----------------------------------------
test_that('make_names_append_list appends suffix to conflicting names', {
  new_list <- list(a = 1, b = 2)
  actual_list <- c('a1', 'a', 'b1', 'b')
  out <- make_names_append_list(new_list, actual_list, suffix = '_test')
  expect_true(all(names(out) %in% c('a_test', 'b_test')))
})

# test f_dec function ---------------------------------------------------------
test_that('f_dec formats numbers', {
  expect_equal(f_dec(1.2345, dig = 0), '1')
  expect_equal(f_dec(1.2345, dig = 2), '1.23')
  expect_equal(f_dec(1.2345, dig = 5), '1.23450')
  expect_equal(f_dec(1.2345, dig = 7), '1.2345000')
})

# test obj type function ------------------------------------------------------
test_that('obj_type returns correct type', {
  expect_equal(obj_type(1.2), 'numeric')
  expect_equal(obj_type(as.Date('2020-01-01')), 'date')
  expect_equal(obj_type(factor('a')), 'factor')
  expect_equal(obj_type(as.raw(10)), 'other')
})

# test make valid cols function -----------------------------------------------
test_that('make_valid_cols handles raw values', {
  expect_type(make_valid_cols(as.raw(1)), 'character')
})

# test data format function ---------------------------------------------------
test_that('test data format function', {
  expect_false(test_data_format(list()))
  expect_true(test_data_format(list(iris)))
  expect_false(test_data_format(list(data.frame())))
})

# test check dir function -----------------------------------------------------
test_that("check_dir creates directory if it does not exist", {
  tmp <- file.path(tempdir(), 'new_dir')
  unlink(tmp, recursive = TRUE)

  expect_false(dir.exists(tmp))
  check_dir(tmp)
  expect_true(dir.exists(tmp))
  unlink(tmp, recursive = TRUE)
})
