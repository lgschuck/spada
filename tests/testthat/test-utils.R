# tests/testthat/test-utils.R

# test safe env function ------------------------------------------------------
test_that('test safe env', {
  e1 <- safe_env()
  expect_identical(parent.env(e1), emptyenv())
  expect_identical(e1 |> length(), 0L)

  e1 <- safe_env('sum')
  expect_identical(parent.env(e1), emptyenv())
  expect_identical(e1 |> length(), 1L)
  expect_identical(e1 |> ls(), 'sum')
  expect_identical(e1$sum, base::sum)
  expect_identical(e1$sum |> class(), 'function')
  expect_identical(e1$sum(1:3), 6L)
})

# test convert function -------------------------------------------------------
df <- data.frame(
  num = as.numeric(1:3),
  int = 1:3L,
  char = c('1', 'a', '1990-01-01'),
  date = as.Date(1:3),
  posix = as.POSIXct(as.Date(1:3)),
  fact = c('a', '3', '1950/01/01'),
  doub = as.double(1:3),
  comp = complex(real = 1:3, imaginary = 2:4)
)

# test convert function - numeric ---------------
test_that('convert function - numeric', {
  # to numeric
  expect_identical(convert(df$num, 'as.numeric'), c(1, 2, 3))

  # to integer
  expect_identical(convert(df$num, 'as.integer'), c(1L, 2L, 3L))

  # to char
  expect_identical(convert(df$num, 'as.character'), c('1', '2', '3'))

  # to date
  expect_identical(convert(df$num, 'as.Date'),
               as.Date(c('1970-01-02', '1970-01-03', '1970-01-04')))
  expect_s3_class(convert(df$num, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$num, 'as.factor'), df$num |> as.factor())

  # to double
  expect_identical(convert(df$num, 'as.double'), c(1, 2, 3))

  # to complex
  expect_identical(convert(df$num, 'as.complex'), c(1+0i, 2+0i, 3+0i))
})

# test convert function - integer ---------------
test_that('convert function - integer', {
  # to numeric
  expect_identical(convert(df$int, 'as.numeric'), c(1, 2, 3))

  # to integer
  expect_identical(convert(df$int, 'as.integer'), c(1L, 2L, 3L))

  # to char
  expect_identical(convert(df$int, 'as.character'), c('1', '2', '3'))

  # to date
  expect_identical(convert(df$int, 'as.Date'),
               as.Date(c('1970-01-02', '1970-01-03', '1970-01-04')))
  expect_s3_class(convert(df$int, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$int, 'as.factor'), df$num |> as.factor())

  # to double
  expect_identical(convert(df$int, 'as.double'), c(1, 2, 3))

  # to complex
  expect_identical(convert(df$int, 'as.complex'), c(1+0i, 2+0i, 3+0i))
})

# test convert function - char ---------------
test_that('convert function - char', {
  # to numeric
  expect_identical(convert(df$char, 'as.numeric'), c(1, NA, NA))

  # to integer
  expect_identical(convert(df$char, 'as.integer'), c(1L, NA, NA))

  # to char
  expect_identical(convert(df$char, 'as.character'), df$char)

  # to date
  expect_identical(convert(df$char, 'as.Date'), as.Date(c(NA, NA, '1990-01-01')))
  expect_s3_class(convert(df$char, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$char, 'as.factor'), df$char |> as.factor())

  # to double
  expect_identical(convert(df$char, 'as.double'), c(1, NA, NA))

  # to complex
  expect_identical(convert(df$char, 'as.complex'), c(1+0i, NA, NA))
})

# test convert function - date ---------------
test_that('convert function - date', {
  # to numeric
  expect_identical(convert(df$date, 'as.numeric'), c(1, 2, 3))

  # to integer
  expect_identical(convert(df$date, 'as.integer'), c(1L, 2L, 3L))

  # to char
  expect_identical(convert(df$date, 'as.character'), c('1970-01-02', '1970-01-03', '1970-01-04'))

  # to date
  expect_identical(convert(df$date, 'as.Date'), as.Date(c(1:3)))
  expect_s3_class(convert(df$date, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$date, 'as.factor'), df$date |> as.factor())

  # to double
  expect_identical(convert(df$date, 'as.double'), c(1, 2, 3))

  # to complex
  expect_identical(convert(df$date, 'as.complex'), c(1+0i, 2+0i, 3+0i))
})

# test convert function - posixct ---------------
test_that('convert function - posixct', {
  # to numeric
  expect_identical(convert(df$posix, 'as.numeric'), c(86400, 172800, 259200))
  # to integer
  expect_identical(convert(df$posix, 'as.integer'), c(86400L, 172800L, 259200L))

  # to char
  expect_identical(convert(df$posix, 'as.character'),
               c('1970-01-02', '1970-01-03', '1970-01-04'))

  # to date
  expect_identical(convert(df$posix, 'as.Date'),
               as.Date(c('1970-01-02', '1970-01-03', '1970-01-04')))
  expect_s3_class(convert(df$posix, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$posix, 'as.factor'), df$posix |> as.factor())

  # to double
  expect_identical(convert(df$posix, 'as.double'), c(86400, 172800, 259200))

  # to complex
  expect_identical(convert(df$posix, 'as.complex'), c(86400+0i, 172800+0i, 259200+0i))
})

# test convert function - factor ---------------
test_that('convert function - factor', {
  # to numeric
  expect_identical(convert(df$fact, 'as.numeric'), c(NA, 3, NA))

  # to integer
  expect_identical(convert(df$fact, 'as.integer'), c(NA, 3L, NA))

  # to char
  expect_identical(convert(df$fact, 'as.character'), c('a', '3', '1950/01/01'))

  # to date
  expect_identical(convert(df$fact, 'as.Date'), as.Date(c(NA, NA, NA)))
  expect_identical(convert(df$fact, 'as.Date', date_format = '%Y/%m/%d'),
               as.Date(c(NA, NA, '1950-01-01')))
  expect_s3_class(convert(df$fact, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$fact, 'as.factor'), df$fact |> as.factor())

  # to double
  expect_identical(convert(df$fact, 'as.double'), c(NA, 3, NA))

  # to complex
  expect_identical(convert(df$fact, 'as.complex'), c(NA, 3+0i, NA))
})

# test convert function - double ---------------
test_that('convert function - double', {
  # to numeric
  expect_identical(convert(df$doub, 'as.numeric'), c(1, 2, 3))

  # to integer
  expect_identical(convert(df$doub, 'as.integer'), c(1L, 2L, 3L))

  # to char
  expect_identical(convert(df$doub, 'as.character'), c('1', '2', '3'))

  # to date
  expect_identical(convert(df$doub, 'as.Date'), as.Date(c('1970-01-02', '1970-01-03', '1970-01-04')))
  expect_s3_class(convert(df$doub, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$doub, 'as.factor'), df$doub |> as.factor())

  # to double
  expect_identical(convert(df$doub, 'as.double'), c(1, 2, 3))

  # to complex
  expect_identical(convert(df$doub, 'as.complex'), c(1+0i, 2+0i, 3+0i))
})

# test convert function - complex ---------------
test_that('convert function - complex', {
  # to numeric
  expect_identical(convert(df$comp, 'as.numeric'), c(1, 2, 3))

  # to integer
  expect_identical(convert(df$comp, 'as.integer'), c(1L, 2L, 3L))

  # to char
  expect_identical(convert(df$comp, 'as.character'), c('1+2i', '2+3i', '3+4i'))

  # to date
  expect_identical(convert(df$comp, 'as.Date'), as.Date(c('1970-01-02', '1970-01-03', '1970-01-04')))
  expect_s3_class(convert(df$comp, 'as.Date'), 'Date')

  # to factor
  expect_identical(convert(df$comp, 'as.factor'), df$comp |> as.factor())

  # to double
  expect_identical(convert(df$comp, 'as.double'), c(1, 2, 3))

  # to complex
  expect_identical(convert(df$comp, 'as.complex'), c(1+2i, 2+3i, 3+4i))
})


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

# test f_dec function ---------------------------------------------------------
test_that('f_dec formats numbers', {
  expect_equal(f_dec(1.2345, dig = 0), '1')
  expect_equal(f_dec(1.2345, dig = 2), '1.23')
  expect_equal(f_dec(1.2345, dig = 5), '1.23450')
  expect_equal(f_dec(1.2345, dig = 7), '1.2345000')
})

# test make var names ---------------------------------------------------------
test_that('make var names', {

  df <- data.frame(
    `v 1` = 1:3,
    `v 1` = 4:6,
    `v 2` = 7:9
  )

  expect_error(make_var_names(1:3))
  expect_identical(make_var_names(df) |> names(), c('v.1', 'v.1.1', 'v.2'))
})

# test all equal --------------------------------------------------------------
test_that('all equal', {
  expect_false(test_all_equal(1:3))
  expect_true(test_all_equal(c(1, 1, 1)))

  expect_equal(test_all_equal(c(NA, NA, NA)), NA)
  expect_equal(test_all_equal(c(NA, 1, 2)), NA)
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

# test make names append list function ----------------------------------------
test_that('make_names_append_list appends suffix to conflicting names', {
  new_list <- list(a = 1, b = 2)
  actual_list <- c('a1', 'a', 'b1', 'b')
  out <- make_names_append_list(new_list, actual_list, suffix = '_test')
  expect_true(all(names(out) %in% c('a_test', 'b_test')))
})

# test is hex color function --------------------------------------------------
test_that('is_hex_color validates hex codes correctly', {
  expect_true(is_hex_color('#FFFFFF'))
  expect_false(is_hex_color('123456'))
})


# test data format function ---------------------------------------------------
test_that('test data format function', {
  expect_false(test_data_format(list()))
  expect_true(test_data_format(list(iris)))
  expect_false(test_data_format(list(data.frame())))
})

# test check dir function -----------------------------------------------------
test_that('check_dir creates directory if it does not exist', {
  tmp <- file.path(tempdir(), 'new_dir')
  unlink(tmp, recursive = TRUE)

  expect_false(dir.exists(tmp))
  check_dir(tmp)
  expect_true(dir.exists(tmp))
  unlink(tmp, recursive = TRUE)
})

# test linear model df output -------------------------------------------------
test_that('linear model df output', {

  s1 <- lm(1:10 ~ rnorm(10)) |> summary()
  expect_error(linear_model_df_output(iris))
  expect_error(linear_model_df_output(1:3))
  expect_s3_class(linear_model_df_output(s1), 'data.frame')
})

# test linear model df metrics ------------------------------------------------
test_that('linear model df metrics', {

  s1 <- lm(1:10 ~ rnorm(10)) |> summary()
  expect_error(linear_model_df_metrics(iris))
  expect_error(linear_model_df_metrics(1:3))
  expect_s3_class(linear_model_df_metrics(s1), 'data.frame')
})

# test desc stats -------------------------------------------------------------
test_that('descriptive stats function', {

  df <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c(10, 20, 30, 40, NA),
    c = factor(c('x', 'x', 'y', 'y', 'z'))
  )

  res <- desc_stats(df, fmt_digits = 2)

  expect_type(res, 'list')
  expect_true(all(c('Mean', 'Gmean', 'Hmean', 'Median', 'Mode',
                    'Min', 'Max', 'IQR', 'Range', 'Variance', 'Standard Deviation',
                    'Skewness', 'Kurtosis') %in% names(res)))

  # Mean
  expect_equal(res$Mean['a'][[1]], mean(df$a, na.rm = TRUE) |> f_num(dig = 2))

  # Gmean
  expect_equal(res$Gmean['a'][[1]], Gmean(df$a, na.rm = TRUE) |> f_num(dig = 2))

  # Hmean
  expect_equal(res$Hmean['a'][[1]], Hmean(df$a, na.rm = TRUE) |> f_num(dig = 2))

  # Median
  expect_equal(as.numeric(res$Median['a']), median(df$a, na.rm = TRUE))

  # Mode (coluna fator deve retornar um valor não vazio)
  expect_equal(res$Mode['c'][[1]], 'x | y')

  # Min e Max
  expect_equal(res$Min['a'][[1]], min(df$a, na.rm = TRUE) |> f_num(dig = 2))
  expect_equal(res$Max['b'][[1]], max(df$b, na.rm = TRUE) |> f_num(dig = 2))

  # IQR
  expect_equal(res[['IQR']]['b'][[1]],
               df$b |> IQR(na.rm = T) |> f_num(dig = 2))

  # Range
  expect_equal(res[['Range']]['b'][[1]],
               paste('[', df$b |> range() |> f_num(dig = 2) , ']', collapse = '--->'))

  # Variance
  expect_equal(res[['Variance']]['b'][[1]],
               df$b |> var(na.rm = T) |> f_num(dig = 2))

  # SD
  expect_equal(res[['Standard Deviation']]['b'][[1]],
               df$b |> sd(na.rm = T) |> f_num(dig = 2))

  # Skewness
  expect_equal(res[['Skewness']]['b'][[1]],
               df$b |> Skew(na.rm = T) |> f_num(dig = 2))

  # Kurtosis
  expect_equal(res[['Kurtosis']]['b'][[1]],
               df$b |> Kurt(na.rm = T) |> f_num(dig = 2))

})

# test spada plot -------------------------------------------------------------
test_that('spada plot function', {
  p <- spada_plot(
    type = 'hist',
    data = data.frame(x = rnorm(100), y = rnorm(100)),
    xvar = 'x'
  )

  expect_s3_class(p, "ggplot")
})

# test output format ----------------------------------------------------------
test_that('test empty list as output', {
  expect_true(test_output_format(list()))
})

test_that('test valid output', {
  valid_output <- list(
    'id1' = list(
      id = '123',
      title = 'Title 1',
      card = shiny::tags$div('content 1')
    ),
    'id2' = list(
      id = '456',
      title = 'itle 2',
      card = shiny::tags$p('content 2')
    )
  )

  expect_true(test_output_format(valid_output))
})

test_that('test other class than list', {
  expect_false(test_output_format('texto'))
  expect_false(test_output_format(123))
})

test_that('test inside elements - not lists', {
  not_list <- list(a = 'test')
  expect_false(test_output_format(not_list))
})

test_that('test missing element in inside list', {
  missing_element <- list(
    'id1' = list(
      id = '123',
      title = 'ok'
    )
  )
  expect_false(test_output_format(missing_element))
})

test_that('test class of inside elements', {
  invalid_output <- list(
    'id1' = list(
      id = 123,  # should be char
      title = 'ok',
      card = shiny::tags$div('content')
    )
  )
  expect_false(test_output_format(invalid_output))
})

# test load conf --------------------------------------------------------------
test_that('no previous conf file', {
  tmpdir <- tempdir()
  conf_path <- file.path(tmpdir, 'conf.RDS')

  if (file.exists(conf_path)) file.remove(conf_path)
  expect_false(file.exists(conf_path))

  start_conf <- c(
    'empty_datasets' = 1,
    'conf_dir' = normalizePath(R_user_dir('spada', 'config'), winslash = '/', mustWork = F),
    'data_dir' = normalizePath(R_user_dir('spada', 'data'), winslash = '/', mustWork = F),
    default_conf
  )

  res <- load_conf(start_conf, tmpdir, themes_names)

  # expect retunr of the start conf since there is no previous file
  expect_equal(res, start_conf)
  expect_true(file.exists(conf_path))
})

test_that('invalid previous conf file', {
  tmpdir <- tempdir()
  conf_path <- file.path(tmpdir, 'conf.RDS')

  valid_conf <- c(
    'empty_datasets' = 1,
    'conf_dir' = normalizePath(R_user_dir('spada', 'config'), winslash = '/', mustWork = F),
    'data_dir' = normalizePath(R_user_dir('spada', 'data'), winslash = '/', mustWork = F),
    default_conf
  )

  invalid_conf <- valid_conf
  invalid_conf$theme <- 'new_theme'
  # save invalid for test
  saveRDS(invalid_conf, conf_path)

  res <- load_conf(valid_conf, tmpdir, themes_names)

  # expect return of the valid conf since the saved is invalid
  expect_equal(res, valid_conf)
})




