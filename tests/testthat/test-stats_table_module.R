# tests/testthat/test-stats_table_module.R

v1 <- c(iris$Petal.Length, NA, NA)
v2 <- c(iris$Sepal.Length, 0, NA)

var1 <- reactive(v1)
var2 <- reactive(v2)
input_percentile <- reactive(90)
percentile <- reactive(quantile(v1, probs = 0.9, na.rm = TRUE))
var1_sd <- reactive(sd(v1, na.rm = TRUE))
pearson_correlation <- reactive(cor(v1, v2, method = 'p', use = 'na.or.complete'))

# test stats table - calculations ---------------------------------------------
test_that('stats table server - test calculations', {

  testServer(
    stats_table_server,
    args = list(
      var1 = var1,
      var2 = var2,
      input_percentile = input_percentile,
      percentile = percentile,
      var1_sd = var1_sd,
      pearson_correlation = pearson_correlation
    ),
    {
      expect_equal(stats_obs(), 152)
      expect_equal(stats_n_nas(), 2)
      expect_equal(stats_min(), mina(v1))
      expect_equal(stats_q1(), p25(v1))
      expect_equal(stats_median(), median(v1, na.rm = T))
      expect_equal(stats_mean(), mean(v1, na.rm = T))
      expect_equal(stats_mode(), fmode(v1, na.rm = T))
      expect_equal(stats_q3(), p75(v1))
      expect_equal(stats_max(), mana(v1))
  })
})

# test stats table - data.frame  and gt table ---------------------------------
test_that('stats table server - test data.frame and gt table', {

  testServer(
    stats_table_server,
    args = list(
      var1 = var1,
      var2 = var2,
      input_percentile = input_percentile,
      percentile = percentile,
      var1_sd = var1_sd,
      pearson_correlation = pearson_correlation
    ),
    {
      expect_s3_class(stats_table(), 'data.frame')
      expect_s3_class(stats_table_fmt(), 'gt_tbl')

      expect_equal(nrow(stats_table()), 11)
      expect_equal(ncol(stats_table()), 2)
      expect_equal(names(stats_table()), c('measure', 'value'))
    })
})
