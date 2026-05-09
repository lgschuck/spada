# tests/testthat/test-frequencies_module.R

# test frequencies - table ----------------------------------------------------
test_that('Tables values - 1 variable', {
  testServer(frequencies_server,
             args = list(reactive(iris$Species)), {

    session$setInputs(btn_freq = 1)

    expect_equal(var(), iris$Species)
    expect_equal(freq_table(), Freq(iris$Species, useNA = 'always') |> as.data.frame())
    expect_s3_class(freq_table(), 'data.frame')
    expect_s3_class(freq_table_gt(), 'gt_tbl')
  })
})
