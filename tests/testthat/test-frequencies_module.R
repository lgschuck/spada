# tests/testthat/test-frequencies_module.R

# test frequencies - table ----------------------------------------------------
test_that('Frequencies', {
  testServer(frequencies_server,
             args = list(reactive(iris$Species), reactive('Species')), {

    session$setInputs(btn_freq = 1)

    expect_equal(var(), iris$Species)

    tab <- Freq(iris$Species, useNA = 'always') |> as.data.frame()
    attr(tab, 'title') <- 'Species'

    expect_equal(freq_table(), tab)
    expect_s3_class(freq_table(), 'data.frame')
    expect_s3_class(freq_table_gt(), 'gt_tbl')
  })
})
