
# ui --------------------------------------------------------------------------
join_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Join', class = 'mini-header'),
      card_body(
        fluidRow(
          column(6, selectInput(ns('dt1_sel'), 'Active Dataset', NULL)),
          column(6, selectInput(ns('dt2_sel'), 'Dataset 2', NULL))
        ),
        radioGroupButtons(
          ns('join_type'),
          'Select a Join',
          c(
            'Left Join' = 'left',
            'Right Join' = 'right',
            'Full Join' = 'full',
            'Inner Join' = 'inner'
            # ,
            # 'Anti Join' = 'anti',
            # 'Semi Join' = 'semi'
          ),
          selected = 'left',
          size = 'sm',
          individual = T
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns('vars_sel1'),
              'Variables 1',
              NULL,
              multiple = T,
              options = list(plugins = list('remove_button', 'clear_button'))
            )
          ),
          column(
            6 ,
            selectizeInput(
              ns('vars_sel2'),
              'Variables 2',
              NULL,
              multiple = T,
              options = list(plugins = list('remove_button', 'clear_button'))
            )
          )
        ),
        radioButtons(
          ns('radio_overwrite'),
          NULL,
          c('New' = 'new', 'Overwrite' = 'overwrite'),
          inline = T
        ),
        conditionalPanel(
          condition = "input.radio_overwrite == 'new'",
          ns = ns,
          textInput(ns('txt_new_dt_name'), 'New Dataset Name', placeholder = 'new_dataset')
        )
      ),
      card_footer(btn_task(ns('btn_apply'), 'Apply', icon('check')))
    ),
    card(
      card_header('Preview', class = 'mini-header'),
      card_body(gt_output(ns('preview_gt'))),
      card_footer(btn_task(
        ns('btn_preview_sample'),
        'Show new sample',
        icon('rotate-right')
      ))
    )
  )
}

# server ----------------------------------------------------------------------
join_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  n_dt <- reactive({
	    req(session$userData$dt$dt)
	    session$userData$dt$dt |> length()
	  })

	  dt_names <- reactive({
	    req(session$userData$dt$dt)
	    session$userData$dt$dt |> names()
	  })

	  act_name <- reactive({
	    req(session$userData$dt$act_name)
	    session$userData$dt$act_name
	  })

	  var1_names <- reactive({
	    req(session$userData$dt$dt)
	    req(input$dt1_sel)
      session$userData$dt$dt[[ input$dt1_sel ]] |> names()
    })

	  var2_names <- reactive({
	    req(session$userData$dt$dt)
	    req(input$dt2_sel)
	    session$userData$dt$dt[[ input$dt2_sel ]] |> names()
	  })

    # update select inputs -------------------
    observe({
      req(act_name())
      updateSelectizeInput(
        session,
        'dt1_sel',
        choices = c(act_name())
      )
    }) |> bindEvent(act_name())

    observe({
      req(var1_names())
      updateSelectizeInput(
        session,
        'vars_sel1',
        choices = c('', var1_names())
      )
    }) |> bindEvent(var1_names())

    observe({
      req(dt_names())

      choices <- dt_names()

      updateSelectizeInput(
        session,
        'dt2_sel',
        choices = choices
      )
    }) |> bindEvent(dt_names())

    observe({
      choices <- if(n_dt() <= 1) character(0) else var2_names()
      updateSelectizeInput(
        session,
        'vars_sel2',
        choices = choices
      )
    }) |> bindEvent(var2_names(), n_dt())

    # # sample to preview conversion -----------
    preview_sample_trigger <- reactiveVal(0)
    preview_sample <- reactive({
      req(input$dt2_sel)

      nrow1 <- get_act_dt(session) |> nrow()
      nrow2 <- session$userData$dt$dt[[ input$dt2_sel ]] |> nrow()

      nrow1 <- if(nrow1 < 8) rep(TRUE, nrow1) else sample(nrow1, 8, replace = F)
      nrow2 <- if(nrow2 < 8) rep(TRUE, nrow2) else sample(nrow2, 8, replace = F)

      list('s_dt1' = get_act_dt(session)[nrow1, ],
           's_dt2' = session$userData$dt$dt[[ input$dt2_sel ]][nrow2, ])
    }) |> bindEvent(preview_sample_trigger(),
                    input$dt1_sel,
                    input$dt2_sel,
                    input$vars_sel1,
                    input$vars_sel2
                    )

    # # update sample in button click ----------
    observe({
      preview_sample_trigger(preview_sample_trigger() + 1)
    }) |> bindEvent(input$btn_preview_sample)

    preview_df <- reactive({
      req(preview_sample())
      req(input$join_type)
      req(input$dt1_sel)
      req(input$dt2_sel)
      req(input$vars_sel1)
      req(input$vars_sel2)

      join_type <- input$join_type
      dt1_name <- input$dt1_sel
      dt2_name <- input$dt2_sel
      vars_dt1 <- input$vars_sel1
      vars_dt2 <- input$vars_sel2

      validate(
        need(dt1_name != dt2_name, 'Select two diferent datasets'),
        need(length(vars_dt1) == length(vars_dt2),
             'The number of selected variables must has the same for both datasets')
      )

      dt1 <- preview_sample()[[ 's_dt1' ]]
      dt2 <- preview_sample()[[ 's_dt2' ]]

      req(vars_dt1 %in% names(dt1))
      req(vars_dt2 %in% names(dt2))

      types_x <- sapply(dt1[, .SD, .SDcols = vars_dt1], obj_type)
      types_y <- sapply(dt2[, .SD, .SDcols = vars_dt2], obj_type)

      validate(
        need(all(types_x == types_y), 'All variables type must match')
      )

      temp <- try(
        dt_join(dt1, dt2, vars_dt1, vars_dt2, join_type), silent = T
      )

      validate(
        need(!inherits(temp, 'try-error'), 'Error')
      )

      temp

    })

    # render preview gt ----------------------
    output$preview_gt <- render_gt({

        preview_df() |>
          gt() |>
          cols_align(align = 'right') |>
          opt_interactive(
            page_size_default = 10,
            use_sorting = F,
            use_pagination = F,
            use_highlight = T,
            use_compact_mode = T) |>
          tab_options(table.background.color = bg_color)
    })

    # apply join --------------------------------------------------------------
    observe({

      join_type <- input$join_type
      dt1_name <- input$dt1_sel
      dt2_name <- input$dt2_sel
      vars_dt1 <- input$vars_sel1
      vars_dt2 <- input$vars_sel2
      radio <- input$radio_overwrite
      new_dt_name <- input$txt_new_dt_name

      # check diferrent datasets
      if((!isTruthy(dt1_name) || !isTruthy(dt2_name)) || dt1_name == dt2_name){
        msg('Select two diferent datasets')
        return()

      # check selected variables
      } else if (!isTruthy(vars_dt1) || !isTruthy(vars_dt2)){
        msg('Select variables for both datasets')
        return()

      # check len of selected variables
      } else if(length(vars_dt1) != length(vars_dt2)){
        msg('The number of selected variables must has the same for both datasets', 4)
        return()

      # check new dt name
      } else if (radio == 'new' && (!is_valid_name(new_dt_name) ||
                                    new_dt_name %in% session$userData$dt_names()))
      {
        msg_error('New name is not valid or already in use')
        return()

      # check vars in datasets
      } else if(
        !all(
          vars_dt1 %in% names(session$userData$dt$dt[[ dt1_name ]]),
          vars_dt2 %in% names(session$userData$dt$dt[[ dt2_name ]])
        ))
        {
        msg_error('Select valid variables')
        return()

      # passed all ui checks
      } else {

        dt1 <- copy(session$userData$dt$dt[[ dt1_name ]])
        dt2 <- copy(session$userData$dt$dt[[ dt2_name ]])

        types_x <- sapply(dt1[, .SD, .SDcols = vars_dt1], obj_type)
        types_y <- sapply(dt2[, .SD, .SDcols = vars_dt2], obj_type)

        if(!all(types_x == types_y)){
          msg_error('The variables types must match')
          return()
        } else {

          temp <- try(
            dt_join(dt1, dt2, vars_dt1, vars_dt2, join_type), silent = T
          )

          # return error msg
          if(inherits(temp, 'try-error')){
            msg('Error: join aborted')
            return()
          # apply join
          } else {
            if(radio == 'overwrite'){
              update_act_dt(session, temp)
            } else if (radio == 'new'){
              append_dt(session, temp, new_dt_name)
              append_meta(session, temp |> df_info(), new_dt_name)
            }
            msg('Join applied')
          }
        }
      }
    }) |> bindEvent(input$btn_apply)
  })
}
