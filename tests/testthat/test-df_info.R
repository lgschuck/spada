# tests/testthat/test-df_info.R

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

  info <- df_info(df)

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

  info <- df_info(df)

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
    zeros <- if (is.numeric(x)) suna(x == 0) else 0
    minv <- if (is.numeric(x)) mina(x) else NA
    maxv <- if (is.numeric(x)) mana(x) else NA

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

  info <- df_info(df)
  info_rbase <- r_base_df_info(df)

  expect_equal(info, info_rbase)
})
