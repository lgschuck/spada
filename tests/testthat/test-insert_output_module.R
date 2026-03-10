# tests/testthat/test-insert_output_module.R

# test insert output ----------------------------------------------------------
test_that('Test insert outuput', {
  testServer(insert_output_server, args = list(reactive(div('Content'))), {

    # Values inserted in modal
    session$setInputs(output_title = 'Title',
                      output_annot = 'Annotation',
                      btn_confirm_add_output = 1)

    # check names
    expect_true(grepl('id_', session$userData$out$elements |> names()))
    # check id
    expect_type(session$userData$out$elements[[1]]$id, 'character')
    expect_true(grepl('id_', session$userData$out$elements[[1]]$id))
    # check title
    expect_type(session$userData$out$elements[[1]]$title, 'character')
    expect_true(session$userData$out$elements[[1]]$title == 'Title')

    # test annotation
    expect_true(inherits(session$userData$out$elements[[1]]$annotation, 'character'))

    # test element
    expect_true(inherits(session$userData$out$elements[[1]]$element, c('shiny.tag', 'gt_tbl')))

    # test card
    expect_true(inherits(session$userData$out$elements[[1]]$card, 'shiny.tag'))

    # test btn_x
    expect_true(inherits(session$userData$out$elements[[1]]$btn_x, 'shiny.tag'))

    # test btn_e
    expect_true(inherits(session$userData$out$elements[[1]]$btn_e, 'shiny.tag'))

  })
})
