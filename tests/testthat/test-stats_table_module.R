# tests/testthat/test-stats_table_module.R

v1 <- c(iris$Petal.Length, NA, NA)
v2 <- c(iris$Sepal.Length, 0, NA)

var <- reactive(v1)
var_name <- reactive('Petal.Length')
input_percentile <- reactive(90)
percentile <- reactive(quantile(v1, probs = 0.9, na.rm = TRUE))
var_sd <- reactive(sd(v1, na.rm = TRUE))
pearson_correlation <- reactive(cor(v1, v2, method = 'p', use = 'na.or.complete'))

# test stats table - calculations ---------------------------------------------
test_that('stats table server - test calculations', {

  testServer(
    stats_table_server,
    args = list(
      var1 = var,
      var1_name = var_name,
      input_percentile = input_percentile,
      percentile = percentile,
      var1_sd = var_sd,
      pearson_correlation = pearson_correlation
    ),
    {
      session$flushReact()

      while(task_stats$status() == 'running'){
        session$flushReact()
      }

      expect_equal(stats_result()$n, 152)
      expect_equal(stats_result()$n_nas, 2)
      expect_equal(stats_result()$min, min_nona(v1))
      expect_equal(stats_result()$q1, quantile(v1, 0.25, na.rm = T))
      expect_equal(stats_result()$median, median(v1, na.rm = T))
      expect_equal(stats_result()$mean, mean(v1, na.rm = T))
      expect_equal(stats_result()$mode, fmode(v1, na.rm = T))
      expect_equal(stats_result()$q3, quantile(v1, 0.75, na.rm = T))
      expect_equal(stats_result()$max, max_nona(v1))
  })
})

# test stats table - data.frame  and gt table ---------------------------------
test_that('stats table server - test data.frame and gt table', {

  testServer(
    stats_table_server,
    args = list(
      var1 = var,
      var1_name = var_name,
      input_percentile = input_percentile,
      percentile = percentile,
      var1_sd = var_sd,
      pearson_correlation = pearson_correlation
    ),
    {
      session$flushReact()

      while(task_stats$status() == 'running'){
        session$flushReact()
      }

      expect_s3_class(stats_table(), 'data.frame')
      expect_s3_class(stats_table_fmt(), 'gt_tbl')

      expect_equal(nrow(stats_table()), 12)
      expect_equal(ncol(stats_table()), 2)
      expect_equal(names(stats_table()), c('measure', 'value'))
      expect_equal(stats_table()[1, 2], var1_name())
    })
})
