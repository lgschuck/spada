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

# test data frame -------------------------------------------------------------
test_that('is_spada_df returns TRUE for pure data.frames', {
  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_true(is_spada_df(df))
})

test_that('is_spada_df returns FALSE for non-data.frame objects', {
  expect_false(is_spada_df(1:5))
  expect_false(is_spada_df(list(a = 1:3, b = 4:6)))
})

test_that('is_spada_df returns TRUE for data.frames with factors and dates', {
  df <- data.frame(
    a = factor(c('x', 'y', 'z')),
    b = as.Date('2020-01-01') + 0:2
  )
  expect_true(is_spada_df(df))
})

test_that('is_spada_df returns FALSE for nested data.table', {

  dt <- data.table::data.table(
    group = c('A', 'B'),
    data = list(
      data.table::data.table(x = 1:2),
      data.table::data.table(x = 3:4)
    )
  )

  expect_false(is_spada_df(dt))
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
  expect_equal(res$Mode['c'][[1]], 'x')

  # Min e Max
  expect_equal(res$Min['a'][[1]], min(df$a, na.rm = TRUE) |> f_num(dig = 2))
  expect_equal(res$Max['b'][[1]], max(df$b, na.rm = TRUE) |> f_num(dig = 2))

  # IQR
  expect_equal(res[['IQR']]['b'][[1]],
               df$b |> IQR(na.rm = T) |> f_num(dig = 2))

  # Range
  expect_equal(res[['Range']]['b'][[1]],
               paste('[', df$b |> range(na.rm = T) |> f_num(dig = 2) , ']', collapse = '--->'))

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
    df = data.frame(x = rnorm(100), y = rnorm(100)),
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
      annotation = 'Annotation 1',
      element = shiny::tags$div('content 1'),
      card = shiny::tags$div('content 1'),
      btn_x = actionButton('b1', 'l1'),
      btn_e = actionButton('b2', 'l2')
    ),
    'id2' = list(
      id = '456',
      title = 'title 2',
      annotation = 'Annotation 1',
      element = shiny::tags$div('content 2'),
      card = shiny::tags$div('content 2'),
      btn_x = actionButton('b1', 'l1'),
      btn_e = actionButton('b2', 'l2')
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
  conf_path <- file.path(tmpdir, 'conf.qs2')

  if (file.exists(conf_path)) file.remove(conf_path)
  expect_false(file.exists(conf_path))

  start_conf <- c(
    'empty_datasets' = 1,
    'conf_dir' = normalizePath(R_user_dir('spada', 'config'), winslash = '/', mustWork = F),
    'data_dir' = normalizePath(R_user_dir('spada', 'data'), winslash = '/', mustWork = F),
    default_conf
  )

  res <- load_conf(start_conf, tmpdir, themes_names)

  # expect return of the start conf since there is no previous file
  expect_equal(res, start_conf)
  expect_true(file.exists(conf_path))
})

test_that('invalid previous conf file', {
  tmpdir <- tempdir()
  conf_path <- file.path(tmpdir, 'conf.qs2')

  valid_conf <- c(
    'empty_datasets' = 1,
    'conf_dir' = normalizePath(R_user_dir('spada', 'config'), winslash = '/', mustWork = F),
    'data_dir' = normalizePath(R_user_dir('spada', 'data'), winslash = '/', mustWork = F),
    default_conf
  )

  invalid_conf <- valid_conf
  invalid_conf$theme <- 'new_theme'
  # save invalid for test
  qs_save(invalid_conf, conf_path)

  res <- load_conf(valid_conf, tmpdir, themes_names)

  # expect return of the valid conf since the saved is invalid
  expect_equal(res, valid_conf)
})

# test summarise_dt function --------------------------------------------------
dt <- iris |> as.data.table()

test_that('summarise dt funciton - distinct', {

  dt_s0 <- dt[, .SD, .SDcols = 'Species'] |> unique()
  dt_s <- summarise_dt(dt, 'distinct', 'Species')

  expect_equal(dt_s0, dt_s)
  expect_s3_class(dt_s, c('data.table', 'data.frame'))
})

test_that('summarise dt funciton - count', {

  dt_s0 <- dt[, .N, by = 'Species']
  dt_s <- summarise_dt(dt, 'count', 'Species')

  expect_equal(dt_s0, dt_s)
  expect_s3_class(dt_s, c('data.table', 'data.frame'))
})

test_that('summarise dt funciton - wrong fun', {
  expect_error(summarise_dt(dt, 'count2', 'Species'))
})

test_that('summarise dt funciton - not data.table', {
  expect_error(summarise_dt(iris, 'count', 'Species'))

})

test_that('summarise dt funciton - not variable', {
  expect_error(summarise_dt(dt, 'count', c('Species', 'Sepal.Length2')))
})

# test update metadata function -----------------------------------------------
dt <- iris |> as.data.table()

test_that('update metadata funciton - wrogn change type', {
  expect_error(update_meta(previous_meta = data.table(), change_type = 'xx'))
})

test_that('update metadata funciton - rename cols', {

  metadata <- list('dt' = dt |> df_info())
  meta0 <- metadata[['dt']]

  dt2 <- copy(dt)
  names(dt2)[names(dt2) == 'Species'] <- 'Species_2'

  new_meta <- update_meta(
    previous_meta = meta0,
    col_names = dt2 |> names(),
    change_type = 'rename_cols'
  )

  meta0 <- dt2 |> df_info()

  expect_equal(meta0, new_meta)
})

test_that('update metadata funciton - select cols', {

  metadata <- list('dt' = dt |> df_info())

  meta0 <- metadata[['dt']]
  dt2 <- subset(dt, select = c('Petal.Width', 'Species'))

  new_meta <- update_meta(
    previous_meta = meta0,
    col_names = dt2 |> names(),
    ncols = ncol(dt2),
    change_type = 'select_cols'
  )

  meta0 <- dt2 |> df_info()

  expect_equal(meta0, new_meta)
})

test_that('update metadata funciton - order cols', {

  metadata <- list('dt' = dt |> df_info())

  meta0 <- metadata[['dt']]
  dt2 <- dt[, c(1, 3, 2, 4, 5)]

  new_meta <- update_meta(
    previous_meta = meta0,
    col_names = dt2 |> names(),
    change_type = 'order_cols'
  )

  meta0 <- dt2 |> df_info()

  expect_equal(meta0, new_meta)
})

test_that('update metadata funciton - convert cols', {

  metadata <- list('dt' = dt |> df_info())

  meta0 <- metadata[['dt']]
  dt2 <- copy(dt)
  dt2$Sepal.Length <- dt2$Sepal.Length |> as.integer()

  new_meta <- update_meta(
    dt = subset(dt2, select = 'Sepal.Length'),
    previous_meta = meta0,
    col_names = dt2 |> names(),
    updated_cols = 'Sepal.Length',
    ncols = dt2 |> ncol(),
    change_type = 'convert_cols'
  )

  meta0 <- dt2 |> df_info()

  expect_equal(meta0, new_meta)
})

# test is_date function -------------------------------------------------------
test_that("is_date - test Sys.Date()", {
  expect_equal(is_date(Sys.Date()), TRUE)
})

test_that("is_date - test Sys.time()", {
  expect_equal(is_date(Sys.time()), TRUE)
})

test_that("is_date - test numeric", {
  expect_equal(is_date(10), FALSE)
})

test_that("is_date - test char", {
  expect_equal(is_date('20'), FALSE)
})

test_that("is_date - test char in Date format", {
  expect_equal(is_date('2024-12-01'), FALSE)
})

test_that('is_date - test only first element of vector', {
  expect_equal(is_date(c(Sys.Date(), 10)), TRUE)
})

test_that('is_date - test only first element of vector - 2', {
  expect_equal(is_date(c(10, Sys.Date())), FALSE)
})

# test is_valid_name function -------------------------------------------------
test_that("is_valid_name - test a valid name", {
  expect_equal(is_valid_name('name'), TRUE)
})

test_that("is_valid_name - test 2 size vector with valid", {
  expect_equal(is_valid_name(c('a', 'b')), c(TRUE, TRUE))
})

test_that("is_valid_name - test name with space", {
  expect_equal(is_valid_name('a a'), FALSE)
})

test_that("is_valid_name - test name starting with number", {
  expect_equal(is_valid_name('1a'), FALSE)
})

test_that("is_valid_name - test name starting with blank space", {
  expect_equal(is_valid_name(' 1a'), FALSE)
})

# test wrapper function from collapse -----------------------------------------
test_that('max_nona - all valid values', {
  expect_equal(max_nona(c(1, 2, 3)), 3)
})

test_that('max_nona - all NA values', {
  expect_equal(max_nona(c(NA, NA, NA)), NA_integer_)
})

test_that('max_nona - all NaN values', {
  expect_equal(max_nona(c(NaN, NaN, NaN)), NA_real_)
})

test_that('max_nona - only n 1 valid out of 3', {
  expect_equal(max_nona(c(1, NA, NA)), 1)
})

test_that('max_nona - only n 2 valid out of 3', {
  expect_equal(max_nona(c(NA, 2, NA)), 2)
})

test_that('max_nona - only n 3 valid out of 3', {
  expect_equal(max_nona(c(NA, NA, 3)), 3)
})

test_that('max_nona - 2 and 3 valid out of 3', {
  expect_equal(max_nona(c(NA, 2, 3)), 3)
})

test_that('max_nona - NA, 3, 2', {
  expect_equal(max_nona(c(NA, 3, 2)), 3)
})

test_that('min_nona - all valid values', {
  expect_equal(min_nona(c(1, 2, 3)), 1)
})

test_that('min_na returns NA', {
  expect_true(is.na(min_na(c(NA, 2, 3))))
})

test_that('min_nona - all NA values', {
  expect_equal(min_nona(c(NA, NA, NA)), NA_integer_)
})

test_that('min_nona - all NaN values', {
  expect_equal(min_nona(c(NaN, NaN, NaN)), NA_real_)
})

test_that('min_nona - only n 1 valid out of 3', {
  expect_equal(min_nona(c(1, NA, NA)), 1)
})

test_that('min_nona - only n 2 valid out of 3', {
  expect_equal(min_nona(c(NA, 2, NA)), 2)
})

test_that('min_nona - only n 3 valid out of 3', {
  expect_equal(min_nona(c(NA, NA, 3)), 3)
})

test_that('min_nona - 2 and 3 valid out of 3', {
  expect_equal(min_nona(c(NA, 2, 3)), 2)
})

test_that('min_nona - NA, 3, 2', {
  expect_equal(min_nona(c(NA, 3, 2)), 2)
})

test_that('min_nona - test is.na all NA', {
  expect_true(is.na(min_nona(c(NA, NA, NA))))
})

test_that('min_nona - works with logical vectors', {
  expect_equal(min_nona(c(TRUE, FALSE, TRUE)), 0)
})

test_that('last_nona - all valid values', {
  expect_equal(last_nona(c(1, 2, 3)), 3)
})

test_that('last_nona - all NA values', {
  expect_equal(last_nona(c(NA, NA, NA)), NA)
})

test_that('last_nona - all NaN values', {
  expect_equal(last_nona(c(NaN, NaN, NaN)), NA_real_)
})

test_that('last_nona - only n 1 valid out of 3', {
  expect_equal(last_nona(c(1, NA, NA)), 1)
})

test_that('last_nona - only n 2 valid out of 3', {
  expect_equal(last_nona(c(NA, 2, NA)), 2)
})

test_that('last_nona - only n 3 valid out of 3', {
  expect_equal(last_nona(c(NA, NA, 3)), 3)
})

test_that('last_nona - 2 and 3 valid out of 3', {
  expect_equal(last_nona(c(NA, 2, 3)), 3)
})

test_that('first_nona - all valid values', {
  expect_equal(first_nona(c(1, 2, 3)), 1)
})

test_that('first_nona - all NA values', {
  expect_equal(first_nona(c(NA, NA, NA)), NA)
})

test_that('first_nona - all NaN values', {
  expect_equal(first_nona(c(NaN, NaN, NaN)), NA_real_)
})

test_that('first_nona - only n 1 valid out of 3', {
  expect_equal(first_nona(c(1, NA, NA)), 1)
})

test_that('first_nona - only n 2 valid out of 3', {
  expect_equal(first_nona(c(NA, 2, NA)), 2)
})

test_that('first_nona - only n 3 valid out of 3', {
  expect_equal(first_nona(c(NA, NA, 3)), 3)
})

test_that('first_nona - 2 and 3 valid out of 3', {
  expect_equal(first_nona(c(NA, 2, 3)), 2)
})

test_that('sum_nona - works with numeric vector without NA values', {
  expect_equal(sum_nona(c(1, 2, 3)), 6)
})

test_that('sum_nona - ignores NA values when na_rm = TRUE (default)', {
  expect_equal(sum_nona(c(NA, 2, 3)), 5)
})

test_that('sum_nona - returns the correct value for a vector with a single element', {
  expect_equal(sum_nona(5), 5)
})

test_that('sum_nona - calculates the sum for a vector with negative values', {
  expect_equal(sum_nona(c(-1, -2, -3)), -6)
})

test_that('sum_nona - calculates the sum for a vector with positive, negative, and zero values', {
  expect_equal(sum_nona(c(-1, 0, 1)), 0)
})

test_that('sum_nona - works with logical vectors (TRUE and FALSE treated as 1 and 0)', {
  expect_equal(sum_nona(c(TRUE, FALSE, TRUE)), 2)
})

test_that('sum_nona - throws an error when given a character vector', {
  expect_error(sum_nona(c('a', 'b', 'c')))
})

# test df_info ----------------------------------------------------------------

test_that('df_info works with a simple data frame', {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c('x', 'y', 'z'),
    c = c(TRUE, FALSE, TRUE)
  )

  info <- df_info(df)

  expect_equal(info$var, c('a', 'b', 'c'))
  expect_equal(info$type, c('double', 'character', 'logical'))
  expect_equal(info$class, c('numeric', 'character', 'logical'))
  expect_equal(info$rows[1], 3)
  expect_equal(info$cols[1], 3)
})

test_that('df_info calculates valid counts and percentages correctly', {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c('x', 'y', NA),
    c = c(TRUE, FALSE, NA)
  )

  info <- df_info(df)

  expect_equal(info$n_valid, c(2, 2, 2))
  expect_equal(info$perc_valid, c(2/3, 2/3, 2/3))
})

test_that('df_info calculates unique counts and percentages correctly', {
  df <- data.frame(
    a = c(1, 2, 2),
    b = c('x', 'y', 'x'),
    c = c(TRUE, FALSE, TRUE)
  )

  info <- df_info(df, type = 'complete')

  expect_equal(info$n_unique, c(2, 2, 2))
  expect_equal(info$perc_unique, c(2/3, 2/3, 2/3))
})

test_that('df_info calculates zero counts and percentages correctly', {
  df <- data.frame(
    a = c(0, 0, 1),
    b = c(0, 1, 2),
    c = c(TRUE, FALSE, FALSE)
  )

  info <- df_info(df)

  expect_equal(info$n_zero, c(2, 1, 0))
  expect_equal(info$perc_zero, c(2/3, 1/3, 0))
})

test_that('df_info calculates NA counts and percentages correctly', {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, NA, 'z'),
    c = c(TRUE, FALSE, NA)
  )

  info <- df_info(df)

  expect_equal(info$n_nas, c(1, 2, 1))
  expect_equal(info$perc_nas, c(1/3, 2/3, 1/3))
})

test_that('df_info handles numeric min and max correctly', {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(10, 20, 30),
    c = c(TRUE, FALSE, TRUE)
  )

  info <- df_info(df)

  expect_equal(info$min, c(1, 10, NA))
  expect_equal(info$max, c(3, 30, NA))
})

test_that('df_info works with an empty data frame', {
  df <- data.frame()

  info <- df_info(df, type = 'complete')

  df0 <- data.table(
    var = 'v1',
    type = NA,
    class = NA,
    size = 0,
    min = NA,
    max = NA,
    n_valid = NA,
    perc_valid = NA,
    n_unique = NA,
    perc_unique = NA,
    n_zero = NA,
    perc_zero = NA,
    n_nas = NA,
    perc_nas = NA,
    rows = NA,
    cols = NA
  )

  expect_equal(nrow(info), 1)
  expect_equal(ncol(info), 16)
  expect_equal(info, df0)
})

test_that('df_info throws an error for non-data-frame inputs', {
  expect_error(df_info(1:10))
})

# df_info compare with r base function ----------------------------------------

r_base_df_info <- function(df) {
  stopifnot(is.data.frame(df))

  if (ncol(df) == 0) {
    return(data.table(
      var = 'v1',
      type = NA,
      class = NA,
      size = 0,
      min = NA,
      max = NA,
      n_valid = NA,
      perc_valid = NA,
      n_unique = NA,
      perc_unique = NA,
      n_zero = NA,
      perc_zero = NA,
      n_nas = NA,
      perc_nas = NA,
      rows = NA,
      cols = NA
    ))
  }

  rows <- nrow(df)
  cols <- ncol(df)

  res <- lapply(seq_len(cols), function(j) {
    x <- df[[j]]

    nas <- sum(is.na(x))
    valid <- rows - nas
    uniq <- length(unique(x))
    zeros <- if (is.numeric(x)) sum_nona(x == 0) else 0
    minv <- if (is.numeric(x)) min_nona(x) else NA
    maxv <- if (is.numeric(x)) max_nona(x) else NA

    list(
      var = names(df)[j],
      type = typeof(x),
      class = paste(class(x), collapse = "/"),
      size = as.numeric(object.size(x)),
      min = minv,
      max = maxv,
      n_valid = valid,
      perc_valid = valid / rows,
      n_unique = uniq,
      perc_unique = uniq / rows,
      n_zero = zeros,
      perc_zero = zeros / rows,
      n_nas = nas,
      perc_nas = nas / rows,
      rows = rows,
      cols = cols
    )
  })

  do.call(rbind.data.frame, res) |> as.data.table()
}

test_that('df_info compare with r base function', {

  df <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, NA, 'z'),
    c = c(TRUE, FALSE, NA)
  )

  info <- df_info(df, type = 'complete')
  info_rbase <- r_base_df_info(df)

  expect_equal(info, info_rbase)
})

# test f_num function ---------------------------------------------------------

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

# test gt info ----------------------------------------------------------------
test_that('Test gt info', {

  gt_info_test <- iris |> df_info() |> gt_info(df_name = 'iris')
  expect_equal(gt_info_test |> class(), c('gt_tbl', 'list'))
  expect_equal(gt_info_test |> length(), 21)

  expect_equal(gt_info_test$`_data`$var, names(iris))
  expect_equal(gt_info_test$`_data`$type, sapply(iris, typeof) |> unname())
  expect_equal(gt_info_test$`_data`$class, sapply(iris, class) |> unname())
  expect_equal(gt_info_test$`_data`$rows, rep(150, 5))
  expect_equal(gt_info_test$`_data`$cols, rep(5, 5))
})
