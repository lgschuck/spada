
# ui --------------------------------------------------------------------------
filter_rows_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Filter Rows', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_filter')),
      selectInput(ns('operator'),
                  'Operator', c('', filter_operators)),
      uiOutput(ns('ui_value'))
    ),
    card_footer(
      btn_task(ns('btn_filter'), 'Apply filters', icon('check'))
    )
  )
}

# server ----------------------------------------------------------------------
filter_rows_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Reactive to get column names
    df_names <- reactive({
      req(input_df())
      input_df() |> names()
    })

    # Store active dataset
    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    output$ui_var_filter <- renderUI(
      selectInput(ns('var'), 'Variable', c('', df_names()))
    )

    # selected column type
    col_type <- reactive({
      req(input$var)
      if(df$df_active[[input$var]] |> is.numeric()) 'numeric'
      else if (df$df_active[[input$var]] |> is_date()) 'date'
      else if (df$df_active[[input$var]] |> is.factor()) 'factor'
      else if (df$df_active[[input$var]] |> is.character()) 'char'
      else if (df$df_active[[input$var]] |> is.logical()) 'logical'
      else if (df$df_active[[input$var]] |> is.complex()) 'complex'
      else 'other'
    })

    # Render UI for value input dynamically
    observe({
      req(input$var, input$operator, df$df_active)
      output$ui_value <- renderUI({
        if (col_type() == 'date'){
          if(input$operator %in% c(equal_operators, compare_operators)){
            dateInput(ns('value'), 'Date')
          } else if (input$operator %in% between_operators){
            dateRangeInput(ns('value'), 'Date', end = Sys.Date() + 30)
          } else if (input$operator %in% in_operators){
            tagList(
              layout_column_wrap(
                dateInput(ns('value'), 'Date'),
                textInput(ns('preview_value'), 'Preview Values')
              ),
              layout_column_wrap(
                btn_task(ns('btn_insert_value'), 'Insert Value', icon('plus')),
                btn_task(ns('btn_clear_value'), 'Clear All Values', icon('xmark'))
              )
            )
          }
        } else if (col_type() == 'numeric'){
          if(input$operator %in% c(equal_operators, compare_operators)){
            numericInput(ns('value'), 'Value', value = 0)
          } else if (input$operator %in% between_operators){
            tagList(
              numericInput(ns('value'), 'Inicial Value', value = 0),
              numericInput(ns('value2'), 'Final Value', value = 0)
            )
          } else if (input$operator %in% in_operators){
            tagList(
              layout_column_wrap(
                numericInput(ns('value'), 'Value', value = 0),
                textInput(ns('preview_value'), 'Preview Values')
              ),
              layout_column_wrap(
                btn_task(ns('btn_insert_value'), 'Insert Value', icon('plus')),
                btn_task(ns('btn_clear_value'), 'Clear All Values', icon('xmark'))
              )
            )
          }
        } else if (col_type() == 'factor'){
          if(input$operator %in% c(equal_operators, in_operators)){
            selectizeInput(
              ns('value'),
              'Value',
              choices = df$df_active[[input$var]] |> levels(),
              multiple = T,
              options = list(create = T)
            )
          }
        } else if (col_type() %in% c('char', 'complex')){
          if(input$operator %in% c(equal_operators, in_operators)){
            selectizeInput(
              ns('value'),
              list('Value', bs_icon("info-circle")) |>
                ttip(PLACE = 'right', 'Text should not be in quotes'),
              choices = NULL,
              multiple = T,
              options = list(create = T)
            )
          }
        } else if (col_type() == 'logical'){
          div()
        } else if(isTruthy(input$var) && isTruthy(input$operator)){
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
    }) |> bindEvent(input$var, input$operator)

    # update selectinput to show pertinent operators
    observe({
      req(input$var)
      updateSelectInput(
        session, 'operator',
        # label = 'Operator',
        choices =
          if(col_type() %in% c('factor', 'char', 'complex')){
            c('',
              equal_operators,
              na_operators,
              in_operators)
          } else if (col_type() == 'logical'){
            c('', logical_operators)
          } else if (col_type() == 'date'){
            c('', filter_operators[filter_operators %notin% logical_operators])
          } else if (col_type() == 'numeric'){
            c('', filter_operators[filter_operators %notin% logical_operators])
          } else { c('', filter_operators) }
      )
    }) |> bindEvent(input$var)

    # Temporary values for multi-inputs
    value_temp <- reactiveValues(value_temp_inserted = NULL)

    # insert values
    observe({
      req(input$value)
      if(is.null(value_temp$value_temp_inserted)){
        value_temp$value_temp_inserted <- input$value
      } else {
        value_temp$value_temp_inserted <- c(value_temp$value_temp_inserted,
                                            input$value)
      }

      updateTextInput(session, 'preview_value',
                      value = value_temp$value_temp_inserted)
    }) |> bindEvent(input$btn_insert_value)

    # clear inserted values
    observe({
      value_temp$value_temp_inserted <- NULL

      updateTextInput(session, 'preview_value',
                      value = '')
    }) |> bindEvent(input$btn_clear_value)

    # apply btn filter rows
    observe({

      # test if var and operator were informed
      if(!isTruthy(input$var)){
        msg_error('Choose a variable')
        return()
      } else if(!isTruthy(input$operator)){
        msg_error('Choose an operator')
        return()
      } else if (!isTruthy(input$value) &
                 input$operator %notin% c(na_operators, logical_operators,
                                          outlier_operators)){
        msg_error('Insert a value')
        return()
      } else if(input$operator %in% between_operators){
        if(col_type() == 'numeric' & !isTruthy(input$value2)){
          msg_error('Inform inicial and final values')
          return()
        } else if(col_type() == 'date' &
                  (!isTruthy(input$value[1]) | !isTruthy(input$value[2]))){
          msg_error('Inform inicial and final dates')
          return()
        }
      } else if(input$operator %in% in_operators &
                col_type() %in% c('date', 'numeric') &
                is.null(value_temp$value_temp_inserted)){
        msg_error('Insert values')
        return()
      }

      # use inserted values
      if(input$operator %in% between_operators){
        if(col_type() == 'numeric'){
          value_temp$value_temp <- c(input$value, input$value2)
        } else if (col_type() == 'date'){
          value_temp$value_temp <- input$value
        }
      } else if (input$operator %in% in_operators &
                 col_type() %notin% c('char', 'complex', 'factor')){
        value_temp$value_temp <- value_temp$value_temp_inserted
      } else {
        value_temp$value_temp <- input$value
      }
      value_temp$len <- value_temp$value_temp |> length()

      # pass values to filter function
      if (input$operator %in%
                 c(na_operators, logical_operators, outlier_operators)){
        df$df_active <- filter_rows(df$df_active,
                                    input$var,
                                    input$operator,
                                    NULL)
        msg('Filter rows: OK')
      } else if(value_temp$len > 1 & input$operator %in%
                c(equal_operators, compare_operators)){
        msg('Operator requires value of length 1')
        return()
      } else {
        df$df_active <- filter_rows(df$df_active,
                                    input$var,
                                    input$operator,
                                    value_temp$value_temp)
        msg('Filter rows: OK')
      }

      # reset value_temp
      value_temp$value_temp_inserted <- NULL
      value_temp$value_temp <- NULL
      value_temp$len <- NULL

      # clear value after click in button
      updateSelectInput(
        session,
        'var',
        selected = ''
      )

      updateSelectInput(
        session,
        'operator',
        selected = ''
      )

    }) |> bindEvent(input$btn_filter)

    return(list(df_filter_rows = reactive(df$df_active),
                btn_filter_rows = reactive(input$btn_filter)))
  })
}
