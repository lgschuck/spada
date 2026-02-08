
# ui --------------------------------------------------------------------------
groupby_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Group By', class = 'mini-header'),
    card_body(
      selectizeInput(ns('vars_groupby'), 'Group by', NULL,
                     multiple = T,
                     width = '80%',
                     options = list(plugins = list('remove_button', 'clear_button'))),
      div(
        layout_column_wrap(
          selectInput(ns('vars_sel'), 'Variables', NULL),
          selectInput(ns('fun'), 'Choose a function', character(0)),
          textInput(ns('txt_new_name'), 'New variable name')
        ),
        layout_column_wrap(
          btn_task(ns('btn_add_var'), 'Add Variable', icon('plus')),
          btn_task(ns('btn_rm_var'), 'Remove Variable', icon('xmark')),
          btn_task(ns('btn_view_var'), 'View New Variables',
                   icon('magnifying-glass'))
        ),
        radioButtons(ns('radio_overwrite'), NULL,
                       c('New' = 'new', 'Overwrite' = 'overwrite'), inline = T
                       ),
        conditionalPanel(
            condition = "input.radio_overwrite == 'new'", ns = ns,
            textInput(ns('txt_new_dt_name'), 'New Dataset Name', placeholder = 'new_dataset')
        )
      )
    ),
    card_footer(btn_task(ns('btn_groupby'), 'Group By', icon('check')))
  )
}

# server ----------------------------------------------------------------------
groupby_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df_names <- reactive(get_act_dt(session) |> names())

	  group <- reactiveValues(
      new_vars = NULL,
      funs = NULL,
      vars = NULL
	  )

	  # variables ---------------------------------------------------------------
	  observe({
	    req(df_names(), input$vars_groupby, session$userData$dt$act_meta())
	    updateSelectInput(
	      session,
	      'vars_sel',
	      choices = df_names()[df_names() %notin% input$vars_groupby]
	    )
	  }) |> bindEvent(input$vars_groupby, session$userData$dt$act_meta())

    # render variables group --------------------------------------------------
	  observe({
	    req(df_names())
	    updateSelectizeInput(
	      session,
	      'vars_groupby',
	      choices = c('', df_names())
	    )
	  }) |> bindEvent(df_names())

    # suggest name for new variable -------------------------------------------
    observe({
      req(input$vars_sel, input$fun)
      updateTextInput(session, 'txt_new_name',
                      value = paste0(input$vars_sel, '_', input$fun))
    }) |> bindEvent(input$vars_sel, input$fun)

    # render functions choices ------------------------------------------------
    selected_var_type <- reactive({
      req(input$vars_sel)
      obj_type(get_act_dt(session)[[input$vars_sel]])
    })

    observe({
      req(selected_var_type())

      choices = switch(
        selected_var_type(),
        'numeric'  = groupby_math_funs,
        'char'     = groupby_char_funs,
        'date'     = groupby_date_funs,
        'logical'  = groupby_logical_funs,
        ' '
      )

      updateSelectInput(
        session,
        'fun',
        choices = choices,
        selected = choices[1]
      )
    }) |> bindEvent(selected_var_type())

    # add and remove buttons --------------------------------------------------
    # add new vars --------
    observe({

      if(!is_valid_name(input$txt_new_name) ||
         input$txt_new_name %in% group$newvars){
        msg('New name is not valid or already inserted')
        return()
      } else if(!(input$fun %in% allowed_operations)) {
        msg_error('Function are not allowed')
        return()
      } else {
        group$newvars <- append(group$newvars, input$txt_new_name)
        group$funs <- append(group$funs, input$fun)
        group$vars <- append(group$vars, input$vars_sel)

        msg('New variable inserted')
      }

    }) |> bindEvent(input$btn_add_var)

    # remove inserted vars -------
    observe({

      if(input$txt_new_name %notin% group$newvars){
        msg('Variable not previously inserted')
      } else {

        index <- which(group$newvars == input$txt_new_name)

        group$newvars <- group$newvars[-index]
        group$funs <- group$funs[-index]
        group$vars <- group$vars[-index]

        msg('Variable removed')
      }

    }) |> bindEvent(input$btn_rm_var)

    # view inserted --------
    observe({
      req(group$newvars, group$funs, group$vars)

      inserted <- build_calls(group$newvars, group$funs, group$vars)

      df <- data.frame(
        new_var = group$newvars,
        operator = rep('=', length(group$newvars)),
        operation = as.character(inserted)[-1]
      )

      df <- df |>
        gt::gt() |>
        cols_label(
          new_var = '',
          operator = '',
          operation = ''
        ) |>
        opt_interactive(
          page_size_default = 5,
          page_size_values = c(5, 10, 25, 50, 100),
          use_resizers = T,
          use_highlight = T,
          use_compact_mode = T,
          use_text_wrapping = F,
          use_page_size_select = T
        ) |>
        tab_options(column_labels.hidden = T)

      showModal(modalDialog(
        title = div(
          h1(icon('layer-group', size = '55px', style = 'margin-right: 8px; color:#02517d'),
             'New Variables'
          )
        ),
        div(df),
        div(p(paste('Group by:', paste0(input$vars_groupby, collapse = ', ')))),
        size = 'l',
        easyClose = T
      ))

    }) |> bindEvent(input$btn_view_var)

    # clean inserted values if change active dataset
    observe({
      req(input$vars_groupby, group$newvars, group$funs)
      req(session$userData$dt$act_meta())

      group$newvars <- NULL
      group$funs <- NULL
      group$vars <- NULL

    }) |> bindEvent(session$userData$dt$act_meta())

    # apply function events ---------------------------------------------------
    observe({
      if(!isTruthy(group$newvars) || !isTruthy(group$vars)) {
        msg('Select at least one variable')
        return()
      } else if(!isTruthy(group$funs)){
        msg('Select a function')
        return()
      } else if(!(any(group$funs %in% allowed_operations))) {
        msg_error('Function are not allowed')
        return()
      }  else if (input$radio_overwrite == 'new' &&
                  (!is_valid_name(input$txt_new_dt_name) ||
                   input$txt_new_dt_name %in% session$userData$dt_names()))
      {
        msg_error('New name is not valid or already in use')
        return()
      } else {
        j_calls <- build_calls(group$newvars, group$funs, group$vars)

        temp <- copy(get_act_dt(session))

        temp <- temp[, j, by = groupby ,
                     env = list(j = j_calls, groupby = input$vars_groupby |> as.list())]


        # overwrite -------
        if(input$radio_overwrite == 'overwrite'){

          update_act_dt(session, temp)

        } else if(input$radio_overwrite == 'new'){

          append_dt(session, temp, input$txt_new_dt_name)
          append_meta(session, temp |> df_info(), input$txt_new_dt_name)

        }
        rm(temp)

        # clean values ---------
        group$newvars <- NULL
        group$funs <- NULL
        group$vars <- NULL

        msg('Group By: OK')
      }

    }) |> bindEvent(input$btn_groupby)
  })
}
