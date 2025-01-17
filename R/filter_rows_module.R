
# ui --------------------------------------------------------------------------
filter_rows_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Filter Rows', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_filter')),
      selectInput(ns('operator'),
                  'Operator', c('', filter_operators)),
      layout_column_wrap(
        uiOutput(ns('ui_value')),
        textInput(ns('txt_preview_value'), 'Preview value')
      ),
    ),
    card_footer(btn_task(ns('btn_filter'), 'Apply filters', icon('check')))
  )
}

# server ----------------------------------------------------------------------
filter_rows_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    output$ui_var_filter <- renderUI(
      selectInput(ns('vars_filter'), 'Variable', c('', df_names()))
    )

    observe({
      output$ui_value <- renderUI({
        if (df$df_active[[input$vars_filter]] |> is_date() &
            input$operator %in% c('==', '!=', '>', '>=', '<', '<=', 'is_na', 'not_na')){
          dateInput(ns('value'), 'Date')
        } else if (df$df_active[[input$vars_filter]] |> is_date() &
                   input$operator %in% c('between', 'not_between')){
          dateRangeInput(ns('value'), 'Date')
        } else {
          selectizeInput(
            ns('value'),
            list('Value', bs_icon("info-circle")) |>
              ttip(PLACE = 'right', 'Text should not be in quotes'),
            choices = NULL,
            multiple = T,
            options = list(create = T)
          )
        }
      })
    }) |> bindEvent(input$vars_filter)

    value_temp <- reactive({

      req(input$value)

      if(df$df_active[[input$vars_filter]] |> is.numeric()){
        unlist(input$value) |> as.numeric()
      } else if (df$df_active[[input$vars_filter]] |> is_date()){
        unlist(input$value) |> as.Date()
      } else if (df$df_active[[input$vars_filter]] |> is.raw()){
        unlist(input$value) |> as.raw()
      } else if (df$df_active[[input$vars_filter]] |> is.complex()){
        unlist(input$value) |> as.complex()
      } else {
        input$value
      }
    })

    # update selectinput to show pertinent operators
    observe({
      updateSelectInput(
        session, 'operator',
        label = 'Operator',
        choices =
          if(df$df_active[[input$vars_filter]] |> is.factor() ||
             df$df_active[[input$vars_filter]] |> is.character() ||
             df$df_active[[input$vars_filter]] |> is.complex()
          ){
            c('',
              '== (Equal)' = '==',
              '!= (Not Equal)' = '!=',
              'Is NA (is.na)' = 'is_na',
              'Not NA (! is.na)' = 'not_na',
              'In (%in%)' = 'in',
              'Not In (! %in%)' = 'not_in')
          } else { c('', filter_operators) }
      )
    }) |> bindEvent(input$vars_filter)

    # update current format txt
    observe({
      updateTextInput(session, 'txt_preview_value',
                      label = 'Preview value',
                      value = value_temp()
      )
    }) |> bindEvent(value_temp())

    # filter rows
    observe({
      if(input$vars_filter == '' || input$operator == ''
         || length(value_temp()) == 0){
        msg('Choose a variable, an operator and a value', 3)
      } else if(length(value_temp()) > 1 & input$operator %in%
                c('==', '!=', '>', '>=', '<', '<=')){
        msg_error('Operator requires value of length 1')
      } else if(length(value_temp()) != 2 & input$operator %in%
                c('between', 'not_between')){
        msg_error('Operator requires value of length 2')
      } else {
        df$df_active <- filter_rows(df$df_active,
                                    input$vars_filter,
                                    input$operator,
                                    value_temp())
        msg('Filter rows: OK')
      }
    }) |> bindEvent(input$btn_filter)

    # update selectize for factors
    observe({
      req(input$vars_filter)
      req(input$operator)

      if(df$df_active[[input$vars_filter]] |> is.factor()){
        updateSelectizeInput(
          session,
          'value',
          label = 'Value',
          choices = df$df_active[[input$vars_filter]] |> levels(),
          selected = ''
        )
      }
    })

    # clear value after click in button
    observe({
      updateSelectizeInput(
        session,
        'value',
        label = 'Value',
        choices = NULL,
        selected = ''
      )
    }) |> bindEvent(input$btn_filter)

    return(list(df_filter_rows = reactive(df$df_active),
                btn_filter_rows = reactive(input$btn_filter)))
  })
}
