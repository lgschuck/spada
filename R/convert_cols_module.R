
# ui --------------------------------------------------------------------------
convert_cols_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Conversions', class = 'mini-header'),
      card_body(
        layout_column_wrap(
          height = '200px',


          # uiOutput(ns('ui_var_sel')),
          selectInput(ns('vars_sel'), 'Variable', NULL),

          textInput(ns('txt_current_format'), 'Current Type / Class')
        ),
        layout_column_wrap(
          height = '800px',
          selectInput(ns('sel_format'), 'Select the new format',
                      c('', 'as.numeric', 'as.integer',
                        'as.character', 'as.Date', 'as.factor',
                        'as.double', 'as.complex')),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'as.Date'", ns('sel_format')),
            selectInput(
              ns('sel_date_formats'),
              list('Choose the entry format', bs_icon('info-circle')) |>
                tooltip('For character and factor inputs'),
              date_formats),
            dateInput(ns('sel_date_origin'),
                      list('Choose a start date', bs_icon('info-circle')) |>
                        tooltip('For numeric inputs'),
                      value = '1970-01-01')
          )
        )
      ),
      card_footer(btn_task(ns('btn_apply'), 'Apply conversion', icon('check')))
    ),
    card(
      card_header('Preview', class = 'mini-header'),
      card_body(gt_output(ns('preview_gt'))),
      card_footer(btn_task(ns('btn_preview_sample'),
                           'Show new sample', icon('rotate-right')))
    )
  )
}

# server ----------------------------------------------------------------------
convert_cols_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    df_names <- reactive(get_act_dt(session) |> names())

    # update select vars input ---------------
    observe({
      req(df_names())
      updateSelectizeInput(
        session,
        'vars_sel',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())

    # current format -------------------------
    current_format <- reactive({
      req(input$vars_sel)
      paste('Type: [', get_act_dt(session)[[input$vars_sel]] |> typeof(), '] |',
            'Class: [',
            paste(get_act_dt(session)[[input$vars_sel]] |> class(), collapse = '/'),
            ']')
    })

    # update current format txt --------------
    observe({
      updateTextInput(session, 'txt_current_format',
                      label = 'Current Type / Class',
                      value = current_format()
      )
    }) |> bindEvent(current_format())

    # sample to preview conversion -----------
    preview_sample_trigger <- reactiveVal(1)
    preview_sample <- reactive({
      nrow_act_dt <- get_act_dt(session) |> nrow()

      if(nrow_act_dt < 8) {
        rep(TRUE, nrow_act_dt)
      } else {
        sample(nrow_act_dt, 8, replace = F)
      }
    }) |> bindEvent(preview_sample_trigger())

    # update sample in button click ----------
    observe({
      preview_sample_trigger(preview_sample_trigger() + 1)
    }) |> bindEvent(input$btn_preview_sample)

    preview_df <- reactive({
      req(input$vars_sel)
      req(input$sel_format)

      if(input$sel_format == 'as.Date'){
        req(input$sel_date_formats)
      }

      preview_df_temp <- subset(
        get_act_dt(session)[preview_sample(), ], select = input$vars_sel)

      preview_df_temp[
        , preview := convert(var1,
                             type = input$sel_format,
                             date_format = input$sel_date_formats,
                             date_origin = input$sel_date_origin),
        env = list(var1 = input$vars_sel)]
    })

    # render preview gt ----------------------
    output$preview_gt <- render_gt({
      req(input$vars_sel)
      if(input$vars_sel %in% df_names()){
        preview_df() |>
          lapply(\(x) if(is.complex(x)) as.character(x) else x) |>
          as.data.frame() |>
          gt() |>
          cols_align(align = 'right') |>
          opt_interactive(
            use_sorting = F,
            use_pagination = F,
            use_highlight = T,
            use_compact_mode = T) |>
          tab_options(table.background.color = bg_color)
      }
    })

    # apply conversions -------------------------------------------------------
    observe({
      if(input$vars_sel == '' | input$sel_format == ''){
        msg('Choose a variable and a new format')
      } else {

        temp <- copy(get_act_dt(session))

        temp[, input$vars_sel :=
                       convert(var1,
                               type = input$sel_format,
                               date_format = input$sel_date_formats,
                               date_origin = input$sel_date_origin),
                     env = list(var1 = input$vars_sel)]

        update_act_dt(session, copy(temp))
        rm(temp)

        msg('Conversion applied')
      }
    }) |> bindEvent(input$btn_apply)
  })
}
